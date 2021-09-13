import java.util.function.*;

interface Sum<A, B> {
    <R> R match(Function<A, R> onLeft, Function<B, R> onRight);
    static <A, B> Sum<A, B> left(A a) { return new Left<A, B>(a); }
    static <A, B> Sum<A, B> right(B b) { return new Right<A, B>(b); }

    static class Left<A, B> implements Sum<A, B> {
        private A a;
        public Left(A aa) { a = aa; }
        public <R> R match(Function<A, R> onLeft, Function<B, R> onRight) {
            return onLeft.apply(a);
        }
    }

    static class Right<A, B> implements Sum<A, B> {
        private B b;
        public Right(B bb) { b = bb; }
        public <R> R match(Function<A, R> onLeft, Function<B, R> onRight) {
            return onRight.apply(b);
        }
    }
}

interface Product<A, B> {
    <R> R unpack(BiFunction<A, B, R> f);
    default A fst() { return unpack((a, b) -> a); }
    default B snd() { return unpack((a, b) -> b); }
    static <A, B> Product<A, B> pack(A a, B b) {
        return new Pack<A, B>(a, b);
    }

    static class Pack<A, B> implements Product<A, B> {
        private A a;
        private B b;
        public Pack(A aa, B bb) { a = aa; b = bb; }
        public <R> R unpack(BiFunction<A, B, R> f) {
            return f.apply(a, b);
        }
    }
}

interface Unit {
    <A> A apply(A a);
    static Unit trivial() {
        return new Trivial();
    }

    static class Trivial implements Unit {
        public <A> A apply(A a) {
            return a;
        }
    }
}

interface Void {
    <A> A absurd();
}

interface Option<A> extends Sum<Unit, A> {
    default <R> R option(R dflt, Function<A, R> f) { return match((u) -> dflt, f); }
    static <A> Option<A> some(A a) {
        return new Some<A>(a);
    }
    static <A> Option<A> none() {
        return new None<A>();
    }

    static class Some<A> extends Sum.Right<Unit, A> implements Option<A> {
        public Some(A a) { super(a); }
    }

    static class None<A> extends Sum.Left<Unit, A> implements Option<A> {
        public None() { super(Unit.trivial()); }
    }
}

interface Equal<A, B> {
    A r2l(B b);
    B l2r(A a);
    static <A> Equal<A, A> refl() { return new Refl<A>(); }

    static class Refl<A> implements Equal<A, A> {
        public A r2l(A a) { return a; }
        public A l2r(A a) { return a; }
    }
}

/*
data Ast :: Type -> Type where
    IntA :: Int -> Ast Int
    IdA :: Int -> Ast a
    LamA :: Ast b -> Ast (a -> b)
    AppA :: Ast (a -> b) -> Ast a -> Ast b

data Ast a
    = (a ~ Int) => IntA Int
    | IdA Int
    | forall x y. (a ~ (x -> y)) => LamA (Ast y)
    | forall x. AppA (Ast (x -> a)) (Ast x)
*/

interface Ast<A> extends
    Sum< Ast.IntA<A>,
        Sum< Ast.IdA<A>,
            Sum< Ast.LamA<?, ?, A>,
                Ast.AppA<?, A>>>> {

    static class IntA<A> extends Sum.Left<Ast.IntA<A>, Sum< Ast.IdA<A>, Sum< Ast.LamA<?, ?, A>, Ast.AppA<?, A>>>> implements Ast<A> {
        public Equal<A, Integer> _eq;
        public int value;
        public IntA(Equal<A, Integer> eq, int value) { super(this); _eq = eq; this.value = value; }
    }

    static class IdA<A> implements Ast<A> {
        public int index;
        public IdA(int index) { this.index = index; }
    }

    static class LamA<A, B, F> implements Ast<F> {
        public Equal<F, Function<A, B>> _eq;
        public Ast<B> body;
        public LamA(Equal<F, Function<A, B>> eq, Ast<B> body) { _eq = eq; this.body = body; }
    }

    static class AppA<A, B> implements Ast<B> {
        public Ast<Function<A, B>> fn;
        public Ast<A> arg;
        public AppA(Ast<Function<A, B>> fn, Ast<A> arg) { this.fn = fn; this.arg = arg; }
    }
}

class Hello {
    public static void main(String[] args) {
        Product<Integer, String> pair = Product.pack(5, "Hello World!");
        System.out.println(pair.snd());
        System.out.println(
            Option.some("Hello World!").option(
                "Impossible!",
                (s) -> { return "Of course!"; }
            )
        );
        Sum<Void, Integer> justInt = Sum.right(42);
        System.out.println(
            justInt.<Integer>match(
                (v) -> { return v.absurd(); },
                (i) -> { return i; }
            )
        );
    }
}
