import { $, _, _0, _1 } from '.';
import { Functor, Applicative } from './mine';

describe('Functor', () => {
    type DoSomethingWithFunctor = <T>(instance: Functor<T>) => <A, B>(value: $<T, [A]>, f: (a: A) => B) => $<T, [B]>;
    const doSomethingWithFunctor: DoSomethingWithFunctor = instance => (value, f) => {
        return instance.map(f, value);
    }

    type MapPlusOne = <T>(instance: Functor<T>) => (value: $<T, [number]>) => $<T, [number]>;
    const mapPlusOne: MapPlusOne = instance => value => {
        return instance.map(n => n + 1, value);
    }

    it('Array', () => {
        const arrayFunctorInstance = Functor<Array<_>>({
            map: (f, array) => {
                const newArray = [];
                for(let i = 0; i < array.length; i++) {
                    newArray[i] = f(array[i]);
                }
                return newArray;
            }
        });

        expect(
            arrayFunctorInstance.map(n => String(n), [1, 2, 3])
        ) .toEqual(
            ["1", "2", "3"]
        );

        expect(
            doSomethingWithFunctor(arrayFunctorInstance)([1, 2, 3], n => n + 1)
        ).toEqual(
            [2, 3, 4]
        )

        expect(
            mapPlusOne(arrayFunctorInstance)([1, 2, 3])
        ).toEqual(
            [2, 3, 4]
        )
    });

    it('BinaryTree', () => {
        type BinaryTree<A> = { type: 'leaf', value: A } | { type: 'branch', left: BinaryTree<A>, right: BinaryTree<A> };
        const leaf = <A>(value: A): BinaryTree<A> => ({ type: 'leaf', value });
        const branch = <A>(left: BinaryTree<A>, right: BinaryTree<A>): BinaryTree<A> => ({ type: 'branch', left, right });

        const treeFunctorInstance: Functor<BinaryTree<_>> = Functor<BinaryTree<_>>({
            map: (f, tree) => {
                switch(tree.type) {
                    case 'leaf':
                        return leaf(f(tree.value));
                    case 'branch':
                        return branch(treeFunctorInstance.map(f, tree.left), treeFunctorInstance.map(f, tree.right));
                }
            }
        });

        expect(
            treeFunctorInstance.map(n => n + 1, leaf(42))
        ).toEqual(
            leaf(43)
        );

        expect(
            treeFunctorInstance.map(
                n => n + 1,
                branch(leaf(1), leaf(2))
            )
        ).toEqual(
            branch(leaf(2), leaf(3))
        );

        expect(
            mapPlusOne(treeFunctorInstance)(
                branch(leaf(1), leaf(2))
            )
        ).toEqual(
            branch(leaf(2), leaf(3))
        );
    });

    it('BTree', () => {
        type BTree<A> = { type: 'leaf', value: A } | { type: 'branch', children: Array<BTree<A>> };
        const leaf = <A>(value: A): BTree<A> => ({ type: 'leaf', value });
        const branch = <A>(children: Array<BTree<A>>): BTree<A> => ({ type: 'branch', children });

        const btreeFunctorInstance: Functor<BTree<_>> = Functor<BTree<_>>({
            map: (f, tree) => {
                switch(tree.type) {
                    case 'leaf':
                        return leaf(f(tree.value));
                    case 'branch':
                        return branch(tree.children.map(child => btreeFunctorInstance.map(f, child)));
                }
            }
        })

        expect(
            btreeFunctorInstance.map(n => n + 1, leaf(42))
        ).toEqual(
            leaf(43)
        );

        expect(
            btreeFunctorInstance.map(
                n => n + 1,
                branch([leaf(1), leaf(2)])
            )
        ).toEqual(
            branch([leaf(2), leaf(3)])
        );

        expect(
            mapPlusOne(btreeFunctorInstance)(
                branch([leaf(1), leaf(2)])
            )
        ).toEqual(
            branch([leaf(2), leaf(3)])
        );
    })

    it('Maybe', () => {
        type Maybe<A> = { type: 'none' } | { type: 'some', value: A };
        const none: Maybe<never> = { type: 'none' };
        const some = <A>(value: A): Maybe<A> => ({ type: 'some', value });

        const maybeFunctorInstance = Functor<Maybe<_>>({
            map: (f, maybe) => {
                switch(maybe.type) {
                    case 'none':
                        return none;
                    case 'some':
                        return some(f(maybe.value))
                }
            }
        });

        expect(
            maybeFunctorInstance.map(n => n + 1, some(42))
        ).toEqual(
            some(43)
        );

        expect(
            maybeFunctorInstance.map((n: number) => n + 1, none)
        ).toEqual(
            none
        );

        type DoSomethingWithMaybeFunctorInstance = <A, B>(value: Maybe<A>, f: (a: A) => B) => Maybe<B>;
        const doSomethingWithMaybeFunctorInstance: DoSomethingWithMaybeFunctorInstance = (value, f) => {
            return maybeFunctorInstance.map(f, value);
        }

        expect(
            doSomethingWithMaybeFunctorInstance(some(3), (n: number) => n + 1)
        ).toEqual(
            some(4)
        );

        expect(
            doSomethingWithFunctor(maybeFunctorInstance)(some(3), (n: number) => n + 1)
        ).toEqual(
            some(4)
        );

        expect(
            mapPlusOne(maybeFunctorInstance)(some(42))
        ).toEqual(
            some(43)
        );
    });

    it('Either e', () => {
        type Either<E, A> = { type: 'left', value: E } | { type: 'right', value: A };
        const left = <E>(value: E): Either<E, never> => ({ type: 'left', value });
        const right = <A>(value: A): Either<never, A> => ({ type: 'right', value });

        const eitherFunctorInstance = Functor<Either<unknown, _>>({
            map: (f, either) => {
                switch(either.type) {
                    case 'left':
                        return either;
                    case 'right':
                        return right(f(either.value));
                }
            }
        });

        const l = eitherFunctorInstance.map(
            n => n + 1,
            left(1)
        )
        l

        expect(
            eitherFunctorInstance.map(
                n => n + 1,
                left(1)
            )
        ).toEqual(
            left(1)
        );

        expect(
            eitherFunctorInstance.map(
                n => n + 1,
                right(1)
            )
        ).toEqual(
            right(2)
        );
    });
});

describe('Applicative', () => {
    it('Maybe', () => {
        type Maybe<A> = { type: 'none' } | { type: 'some', value: A };
        const none: Maybe<never> = ({ type: 'none' });
        const some = <A>(value: A): Maybe<A> => ({ type: 'some', value });

        const maybeApplicativeInstance = Applicative<Maybe<_>>({
            pure: some,
            ap: (maybeF, maybeA) => {
                return maybeF.type === 'none'
                    ? none
                    : maybeA.type === 'none'
                        ? none
                        : some(maybeF.value(maybeA.value))
            }
        });

        expect(
            maybeApplicativeInstance.pure(3)
        ).toEqual(
            some(3)
        );

        expect(
            maybeApplicativeInstance.ap(some((n: number) => n + 1), some(42))
        ).toEqual(
            some(43)
        )

        expect(
            maybeApplicativeInstance.map(n => n + 1, some(42))
        ).toEqual(
            some(43)
        );

        type Map3 = <T>(instance: Applicative<T>) => <A, B, C, D>(
            f: (a: A) => (b: B) => (c: C) => D,
            a: $<T, [A]>,
            b: $<T, [B]>,
            c: $<T, [C]>
        ) => $<T, [D]>;
        const map3: Map3 = i => (f, a, b, c) => i.ap(i.ap(i.ap(i.pure(f), a), b), c)

        expect(
            map3(maybeApplicativeInstance)(a => b => c => a + b + c, some(2), some(5), some(7))
        ).toEqual(
            some(14)
        )

        expect(
            map3(maybeApplicativeInstance)(a => (b: number) => c => a + b + c, some(2), none, some(7))
        ).toEqual(
            none
        )

        type Map4 = <T>(instance: Applicative<T>) => <A, B, C, D, E>(
            f: (a: A) => (b: B) => (c: C) => (d: D) => E,
            a: $<T, [A]>,
            b: $<T, [B]>,
            c: $<T, [C]>,
            d: $<T, [D]>
        ) => $<T, [E]>;
        const map4: Map4 = ({ ap, pure }) => (f, a, b, c, d) => ap(ap(ap(ap(pure(f), a), b), c), d);

        expect(
            map4(maybeApplicativeInstance)(a => b => c => d => a + b + c + d, some(1), some(2), some(3), some(4))
        ).toEqual(
            some(10)
        )

        interface P<A> {
            v: A,
            p: <B>(f: (a: A) => B) => P<B>;
        }
        const P = <A>(a: A): P<A> => ({
            v: a,
            p: f => P(f(a))
        })

        const flipc = <A, B, C>(f: (a: A, b: B) => C) => (b: B) => (a: A) => f(a, b);
        const flippedAp = flipc(maybeApplicativeInstance.ap)

        expect(
            P(maybeApplicativeInstance.pure((a: number) => (b: number) => a + b)).
                p(flippedAp(some(1))).
                p(flippedAp(some(4))).
                v
        ).toEqual(
            some(5)
        )

        const ApplicativePipe = <T>(i: Applicative<T>) => {
            return <F extends (a: any) => any>(f: F) => {
                const pureF = i.pure(f);

                const Pipe = <F extends (a: any) => any>(f: $<T, [F]>) => {
                    type A = F extends (a: infer A) => any ? A : never;
                    type R = F extends (a: any) => infer R ? R : never;

                    return {
                        value: f,
                        pipe: (a: $<T, [A]>) => {
                            return Pipe(i.ap<A, R>(f, a))
                        }
                    }
                }

                return Pipe(pureF);
            }
        }

        expect(
            ApplicativePipe(maybeApplicativeInstance)(
                (a: number) => (b: number) => a + b
            ).
                pipe(some(1)).
                pipe(some(4)).
                value
        ).toEqual(
            some(5)
        )
    })

    it("Pipe", () => {
        interface Pipe<A> {
            v: A,
            p: <B>(f: (a: A) => B) => Pipe<B>;
        }
        const Pipe = <A>(a: A): Pipe<A> => ({
            v: a,
            p: f => Pipe(f(a))
        })

        const plus1 = (n: number) => n + 1;
        const times2 = (n: number) => n * 2;

        expect(
            Pipe(2).p(plus1).p(times2).v
        ).toEqual(
            6
        )
    })
});
