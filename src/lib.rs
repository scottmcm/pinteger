// Copyright 2017
// Licensed under the Academic Free License version 3.0

#![cfg_attr(not(feature = "std"), no_std)]

#![cfg_attr(feature = "nightly", feature(core_intrinsics))]
#![cfg_attr(feature = "nightly", feature(i128_type))]
#![cfg_attr(feature = "nightly", feature(try_from))]

#[cfg(not(feature = "std"))]
use core as std_or_core;
#[cfg(feature = "std")]
use std as std_or_core;

use std_or_core::{cmp, fmt, mem, ops};

#[cfg(feature = "nightly")]
use std_or_core::convert::{TryFrom, TryInto};

mod internals {
    pub trait Pint {
        type Inner;
    }
}
use internals::Pint;

macro_rules! impl_math_for_copy {
    ($tr:ident & $atr:ident for $name:ident as $f:ident & $fa:ident $op:tt) => {
        impl<'a> ops::$tr for &'a $name {
            type Output = $name;
            fn $f(self, y: Self) -> Self::Output {
                *self $op *y
            }
        }
        impl ops::$atr for $name {
            fn $fa(&mut self, y: Self) {
                *self = *self $op y;
            }
        }
        impl<'a> ops::$atr<&'a $name> for $name {
            fn $fa(&mut self, y: &'a Self) {
                *self = *self $op *y;
            }
        }
    }
}

macro_rules! define_pinteger { ( $name:ident $inner:ty ) => {

    #[allow(non_camel_case_types)]
    #[derive(Copy, Clone, Eq, PartialEq)]
    pub struct $name { raw: $inner }

    impl Pint for $name {
        type Inner = $inner;
    }

    impl $name {
        const ONE: $name = $name { raw: 1 };
        pub const MIN: $name = $name { raw: 1 };
        pub const MAX: $name = $name { raw: 0 };

        #[inline]
        pub fn one_plus(x: $inner) -> Self {
            $name { raw: x.wrapping_add(1) }
        }

        #[inline]
        pub fn minus_one(self) -> $inner {
            self.raw.wrapping_sub(1)
        }

        #[inline]
        pub fn checked_from(x: $inner) -> Option<Self> {
            if x == 0 { None }
            else { Some($name { raw: x }) }
        }

        fn bits() -> u32 {
            mem::size_of::<$inner>() as u32 * 8
        }

        #[cfg(not(feature = "nightly"))]
        #[inline]
        pub fn ilog2(self) -> u32 {
            let z = self.raw.leading_zeros();
            if self.raw == 0 { z }
            else { Self::bits() - 1 - z }
        }

        #[cfg(feature = "nightly")]
        #[inline]
        pub fn ilog2(self) -> u32 {
            let x = self.raw;
            if x == 0 {
                Self::bits()
            } else {
                let z = unsafe { std_or_core::intrinsics::ctlz_nonzero(x) as u32 };
                Self::bits() - 1 - z
            }
        }

        #[inline]
        pub fn is_power_of_two(self) -> bool {
            (self.raw.wrapping_sub(1) & self.raw) == 0
        }
    }

    impl Ord for $name {
        fn cmp(&self, y: &Self) -> cmp::Ordering {
            Ord::cmp(&self.minus_one(), &y.minus_one())
        }
    }
    impl PartialOrd for $name {
        fn partial_cmp(&self, y: &Self) -> Option<cmp::Ordering> {
            Some(Ord::cmp(self, y))
        }
    }

    impl ops::Add for $name {
        type Output = Self;
        fn add(self, y: Self) -> Self::Output {
            $name::one_plus(self.minus_one() + y.minus_one() + 1)
        }
    }

    impl ops::Sub for $name {
        type Output = Self;
        fn sub(self, y: Self) -> Self::Output {
            $name::one_plus(self.minus_one() - y.minus_one() - 1)
        }
    }

    impl ops::Mul for $name {
        type Output = Self;
        fn mul(self, y: Self) -> Self::Output {
            // Yes, LLVM is smart enough to turn all this
            // into just `mul` in release mode.
            let x = self.minus_one();
            let y = y.minus_one();
            $name::one_plus(x + x*y + y)
        }
    }

    impl ops::Div for $name {
        type Output = Self;
        fn div(self, y: Self) -> Self::Output {
            let (x, y) = (self.raw, y.raw);
            if x == 0 {
                if y == 0 {
                    Self::ONE
                } else {
                    Self::one_plus(<$inner>::wrapping_sub(0, y) / y)
                }
            } else {
                debug_assert!(y != 0 && x >= y,
                    "attempt to divide with underflow");
                if y == 0 {
                    Self::MAX
                } else {
                    Self { raw: x / y }
                }
            }
        }
    }

    impl_math_for_copy!(Add & AddAssign for $name as add & add_assign +);
    impl_math_for_copy!(Sub & SubAssign for $name as sub & sub_assign -);
    impl_math_for_copy!(Mul & MulAssign for $name as mul & mul_assign *);
    impl_math_for_copy!(Div & DivAssign for $name as div & div_assign /);

    impl ops::Div<$name> for $inner {
        type Output = $inner;
        fn div(self, y: $name) -> Self::Output {
            if y.raw == 0 {
                0
            } else {
                self / y.raw
            }
        }
    }
    impl ops::Rem<$name> for $inner {
        type Output = $inner;
        fn rem(self, y: $name) -> Self::Output {
            if y.raw == 0 {
                self
            } else {
                self % y.raw
            }
        }
    }
}}

define_pinteger!(p8 u8);
define_pinteger!(p16 u16);
define_pinteger!(p32 u32);
define_pinteger!(p64 u64);
#[cfg(feature = "nightly")]
define_pinteger!(p128 u128);
define_pinteger!(psize usize);

macro_rules! impl_pinteger_try_from {
    ( $larger:ty => $($smaller:ty)+ ) => {$(
        #[cfg(feature = "nightly")]
        impl TryFrom<$larger> for $smaller {
            type Error = <<$larger as Pint>::Inner as TryInto<<$smaller as Pint>::Inner>>::Error;
            fn try_from(x: $larger) -> Result<$smaller, Self::Error> {
                match x.minus_one().try_into() {
                    Ok(v) => Ok(<$smaller>::one_plus(v)),
                    Err(e) => Err(e),
                }
            }
        }
    )+}
}

macro_rules! impl_pinteger_from {
    ( $smaller:ty => $($larger:ty)+ ) => {$(
        impl From<$smaller> for $larger {
            fn from(x: $smaller) -> $larger {
                <$larger>::one_plus(x.minus_one().into())
            }
        }
        impl_pinteger_try_from!($larger => $smaller);
    )+}
}
impl_pinteger_from!(p8 => p16 p32 p64 psize);
impl_pinteger_from!(p16 => p32 p64);
impl_pinteger_from!(p32 => p64);

impl_pinteger_try_from!(psize => p16 p32 p64);

macro_rules! impl_pinteger_from_larger {
    ( $smaller:ty => $($larger:ty)+ ) => {$(
        impl From<$smaller> for $larger {
            fn from(x: $smaller) -> $larger {
                <$larger>::from(x.minus_one()) + 1
            }
        }
    )+}
}
impl_pinteger_from_larger!(p8 => u16 u32 u64);
impl_pinteger_from_larger!(p16 => u32 u64);
impl_pinteger_from_larger!(p32 => u32 u64);

macro_rules! impl_pinteger_debug {
    ( $name:ident $max:expr ) => {
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                if self.raw == 0 {
                    f.write_str($max)
                } else {
                    <<$name as Pint>::Inner as fmt::Debug>::fmt(&self.raw, f)
                }
            }
        }
    }
}
impl_pinteger_debug!(p8 "256");
impl_pinteger_debug!(p16 "65536");
impl_pinteger_debug!(p32 "4294967296");
impl_pinteger_debug!(p64 "18446744073709551616");
#[cfg(target_pointer_width = "32")]
impl_pinteger_debug!(psize "4294967296");
#[cfg(target_pointer_width = "64")]
impl_pinteger_debug!(psize "18446744073709551616");

#[cfg(feature = "nightly")]
mod i128_impls {
    use super::*;

    impl_pinteger_from!(p8 => p128);
    impl_pinteger_from!(p16 => p128);
    impl_pinteger_from!(p32 => p128);
    impl_pinteger_from!(p64 => p128);

    impl_pinteger_try_from!(psize => p128);

    impl_pinteger_from_larger!(p8 => u128);
    impl_pinteger_from_larger!(p16 => u128);
    impl_pinteger_from_larger!(p32 => u128);
    impl_pinteger_from_larger!(p64 => u128);

    impl_pinteger_debug!(p128 "340282366920938463463374607431768211456");
}

#[cfg(test)]
mod tests {
    use ::*;

    #[test]
    fn consts_min_max() {
        assert!(p32::MIN != p32::MAX);
        assert!(p32::MIN < p32::MAX);
    }

    #[test]
    fn math() {
        assert_eq!( p32::checked_from(13 + 5).unwrap(),
            p32::checked_from(13).unwrap() + p32::checked_from(5).unwrap());
        assert_eq!( p32::checked_from(13 - 5).unwrap(),
            p32::checked_from(13).unwrap() - p32::checked_from(5).unwrap());
        assert_eq!( p32::checked_from(13 * 5).unwrap(),
            p32::checked_from(13).unwrap() * p32::checked_from(5).unwrap());
        assert_eq!( p32::checked_from(13 / 5).unwrap(),
            p32::checked_from(13).unwrap() / p32::checked_from(5).unwrap());

        assert_eq!(p32::ONE, p32::ONE / p32::ONE);
        assert_eq!(p32::ONE, p32::MAX / p32::MAX);
        assert_eq!(p32::MAX, p32::MAX / p32::ONE);

        assert_eq!(p8::checked_from(128).unwrap(),
            p8::MAX / p8::checked_from(2).unwrap());
    }

    #[test]
    fn ilog2() {
        assert_eq!(0, p64::MIN.ilog2());
        for i in 1..64 {
            assert_eq!(p64::checked_from(1 << i).unwrap().ilog2(), i, "1 << {}", i);
            assert_eq!(p64::checked_from((1 << i) + 1).unwrap().ilog2(), i, "1<<{} + 1", i);
            assert_eq!(p64::checked_from((1 << i) - 1).unwrap().ilog2(), i-1, "1<<{} - 1", i);
        }
        assert_eq!(64, p64::MAX.ilog2());
    }
}
