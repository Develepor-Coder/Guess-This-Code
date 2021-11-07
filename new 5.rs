#![feature(generic_associated_types)]

use std::marker::PhantomData;

macro_rules! num {
    () => { Z };
    (* $($rest:tt)*) => { S<num!($($rest)*)> };
}

macro_rules! print_gcd {
    ($M:ty, $N:ty) => {
        println!("gcd({}, {}) = {}", <$M>::default(), <$N>::default(), <($M, $N) as Gcd>::Output::default())
    };
}

// Compile-time assertion that gcd(M, N) == D
macro_rules! assert_gcd_eq {
    (($M:ty, $N:ty) == $D:ty) => {
        fn __inner() where <($M, $N) as Gcd>::Output: From<$D> {}
    };
}

fn main() {
    print_gcd!(num!(*******), num!(*****));

    // assert_gcd_eq!((num!(*******), num!(*****)) == num!(*));
}

struct True;
struct False;

trait Bool {}
impl Bool for True {}
impl Bool for False {}

/// This type represents 0.
#[derive(Default)]
struct Z;

/// This type is required for some of the associated types to work properly.
/// It (kind of) represents -1.
#[derive(Default)]
struct ZPred;

/// This type represents the successor function, and allows us to construct
/// the natural numbers.
///
/// For example, S\<Z> represents the first successor of 0, or 1. \
/// S<S<S\<Z>>> represents the third successor of 0, or 3.
///
/// In general,                        \
/// S<S<...<S\<Z>>...> represents `N`. \
/// |_________|                        \
/// N
#[derive(Default)]
struct S<N: Nat>(PhantomData<N>);

/// The predecessor of N, i.e. N-1
type P<N> = <N as Nat>::Pred;

trait Nat: Default {
    /// Computes `Self - 1`
    type Pred: Nat;

    /// [True] iff `Self > 0`.
    type IsPositive: Bool;

    /// [True] iff `Self < M`
    type LessThan<M: Nat>: Bool;

    /// Computes `Self + M`
    type Add<M: Nat>: Nat;

    /// Computes `|Self - M|`
    type AbsDiff<M: Nat>: Nat;

    /// Computes `Self * Self`
    type Squared: Nat;

    /// Computes `Floor(Self / 2)`
    type Halved: Nat;

    /// Computes `Self * M`
    type Mul<M: Nat>: Nat;
}

/// Computes `(M+N)^2`
type SquaredSum<M, N> = <<M as Nat>::Add<N> as Nat>::Squared;

/// Computes `M^2 + N^2`
type SumSquared<M, N> = <<M as Nat>::Squared as Nat>::Add<<N as Nat>::Squared>;

impl Nat for Z {
    type Pred = ZPred;
    type IsPositive = False;
    type LessThan<M: Nat> = M::IsPositive;

    // 0 + M = M
    type Add<M: Nat> = M;

    // |0 - M| = M
    type AbsDiff<M: Nat> = M;

    type Squared = Z;
    type Halved = Z;

    // 0 * M = 0
    type Mul<M: Nat> = Z;
}

impl Nat for ZPred {
    type Pred = Z;
    type IsPositive = False;
    type LessThan<M: Nat> = True;
    type Add<M: Nat> = P<M>;
    type AbsDiff<M: Nat> = S<M>;
    type Squared = S<Z>;
    type Halved = Z;
    type Mul<M: Nat> = Z;
}

impl<N: Nat> Nat for S<N> {
    type Pred = N;
    type IsPositive = True;

    // M < N <=> (M-1) < (N-1)
    type LessThan<M: Nat> = N::LessThan<P<M>>;

    // M + N = 1 + (M + (N-1))
    type Add<M: Nat> = S<N::Add<M>>;

    // |M - N| = |(N-1) - (M-1)|
    type AbsDiff<M: Nat> = <P<M> as Nat>::AbsDiff<N>;

    // (N-1)^2 = N^2 - 2N + 1
    // -> N^2 = (N-1)^2 + N + (N-1)
    type Squared = <N::Squared as Nat>::Add<Self::Add<N>>;

    // N/2 = (N-2+2)/2
    // -> N/2 = 1 + (N-2)/2
    type Halved = S<<P<N> as Nat>::Halved>;

    // (M+N)^2 = M^2 + 2MN + N^2
    // -> MN = ((M+N)^2 - M^2 - N^2) / 2
    type Mul<M: Nat> = <<SumSquared<M, Self> as Nat>::AbsDiff<SquaredSum<M, Self>> as Nat>::Halved;
}

/// For natural numbers `M` and `N` with `N != 0`, computes `Floor(M / N)`
trait FloorDiv {
    type Output: Nat;
}

impl<M: Nat> FloorDiv for (M, Z) {
    type Output = ZPred;
}

impl<N: Nat> FloorDiv for (Z, S<N>) {
    type Output = Z;
}

impl<M: Nat, N: Nat> FloorDiv for (S<M>, S<N>)
where
    (S<M>, S<N>): __FloorDiv<M::LessThan<N>>,
{
    type Output = <(S<M>, S<N>) as __FloorDiv<M::LessThan<N>>>::Output;
}

trait __FloorDiv<B: Bool> {
    type Output: Nat;
}

impl<M: Nat, N: Nat> __FloorDiv<True> for (M, S<N>) {
    type Output = Z;
}

/// For positive natural numbers `M` and `N` with `M >= N`, `(M, N)` --> `(M-N, N)`
type PairAfterSub<M, N> = (<M as Nat>::AbsDiff<N>, S<N>);

/// For positive natural numbers `M` and `N`, [True] iff `M-N < N`
type LessAfterSub<M, N> = <<M as Nat>::AbsDiff<N> as Nat>::LessThan<S<N>>;

impl<M: Nat, N: Nat> __FloorDiv<False> for (S<M>, S<N>)
where
    PairAfterSub<M, N>: __FloorDiv<LessAfterSub<M, N>>,
{
    // Recurse: Floor(M / N) = 1 + Floor((M-N) / N)
    type Output = S<<PairAfterSub<M, N> as __FloorDiv<LessAfterSub<M, N>>>::Output>;
}

type Quotient<M, N> = <(M, N) as FloorDiv>::Output;

/// For positive natural numbers `M` and `N` with `N != 0`, computes `M - N * Floor(M / N)`,
/// i.e. the remainder when `M` is divided by `N`.
type Remainder<M, N> = <M as Nat>::AbsDiff<<N as Nat>::Mul<Quotient<M, N>>>;

/// For natural number `M` and `N`, computes `gcd(M, N)`.
trait Gcd {
    type Output: Nat;
}

impl<N: Nat> Gcd for (Z, S<N>) {
    type Output = Z;
}

impl<M: Nat> Gcd for (M, Z) {
    type Output = M;
}

/// For positive natural numbers `M` and `N` with `N != 0`, `(M, N) --> (N, Remainder(M / N))
type NextPair<M, N> = (S<N>, Remainder<S<M>, S<N>>);

impl<M: Nat, N: Nat> Gcd for (S<M>, S<N>)
where
    (S<M>, S<N>): FloorDiv,
    NextPair<M, N>: Gcd,
{
    // Recurse: gcd(M, N) = gcd(N, M % N)
    type Output = <NextPair<M, N> as Gcd>::Output;
}

mod pretty_print {
    use super::*;
    use std::fmt;

    pub trait IntValue {
        fn int_value() -> u64;
    }

    impl IntValue for Z {
        fn int_value() -> u64 {
            0
        }
    }

    impl<N: Nat + IntValue> IntValue for S<N> {
        fn int_value() -> u64 {
            1 + N::int_value()
        }
    }

    impl fmt::Display for Z {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", Self::int_value())
        }
    }

    impl<N: Nat + IntValue> fmt::Display for S<N> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", Self::int_value())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_types_eq {
        ($($T:ty, $U:ty $(,)?);+ $(;)?) => {
            fn __inner() where $($T: From<$U>),+ {}
        };
    }

    #[test]
    fn test_abs_diff() {
        assert_types_eq!(
            <num!(****) as Nat>::AbsDiff<num!(**)>, num!(**);
            <num!(**) as Nat>::AbsDiff<num!(****)>, num!(**);
            <num!() as Nat>::AbsDiff<num!()>, num!();
            <num!(*****) as Nat>::AbsDiff<num!(*****)>, num!();
        );
    }

    #[test]
    fn test_floor_div() {
        assert_types_eq!(
            <(num!(*******), num!(**)) as FloorDiv>::Output, num!(***);
            <(num!(*****), num!(**)) as FloorDiv>::Output, num!(**);
            <(num!(********), num!(***)) as FloorDiv>::Output, num!(**);
            <(num!(**), num!(*****)) as FloorDiv>::Output, num!();
        );
    }

    #[test]
    fn test_add() {
        assert_types_eq!(
            <num!(***) as Nat>::Add<num!(**)>, num!(*****);
        );
    }

    #[test]
    fn test_mul() {
        assert_types_eq!(
            <num!(***) as Nat>::Mul<num!(****)>, num!(************);
            <num!(*****) as Nat>::Mul<num!()>, num!();
            <num!(*) as Nat>::Mul<num!(****)>, num!(****);
        );
    }

    #[test]
    fn test_squared() {
        assert_types_eq!(
            <num!() as Nat>::Squared, num!();
            <num!(*) as Nat>::Squared, num!(*);
            <num!(**) as Nat>::Squared, num!(****);
            <num!(****) as Nat>::Squared, num!(****************);
        );
    }

    #[test]
    fn test_halved() {
        assert_types_eq!(
            <num!(******) as Nat>::Halved, num!(***);
            <num!(*) as Nat>::Halved, num!(*);
            <num!(*********) as Nat>::Halved, num!(*****);
        );
    }

    #[test]
    fn test_gcd() {
        assert_types_eq!(
            <(num!(************), num!(********)) as Gcd>::Output, num!(****);
            <(num!(*******), num!(*****)) as Gcd>::Output, num!(*);
        );
    }
}