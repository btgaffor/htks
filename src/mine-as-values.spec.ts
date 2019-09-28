import { $ } from '.';

describe("Functor", () => {
    interface Functor<A> {
        map: <B>(f: (x: A) => B) => Functor<B>;
    }
    const useFunctor = (a: Functor<number>): Functor<string> => a.map(n => n + "hi")

    it("Maybe", () => {
        class Some<A> implements Functor<A> {
            value: A;
            constructor(value: A) {
                this.value = value;
            }
            map<B>(f: (x: A) => B) {
                return new Some(f(this.value))
            }
        }
        class None implements Functor<any> {
            map() {
                return this;
            }
        }
        type Maybe<A> = Some<A> | None
        const some = <A>(a: A): Maybe<A> => new Some<A>(a)
        const none = <A = any>(): Maybe<A> => new None()

        const actual = some(4).map(n => n + 3)
        expect(actual instanceof Some).toEqual(true);
        if (actual instanceof Some) {
            expect(actual.value).toEqual(7);
        }

        const a: Maybe<number> = none();

        const empty = a.map(n => n + 3);
        expect(empty instanceof None).toEqual(true);

        expect(useFunctor(some(3))).toEqual(some("3hi"))
    });
});
