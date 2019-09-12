import { $, _, _0, _1 } from '.';
import { Functor, Applicative } from './mine';
import { Monad } from './monad/Monad';
import { Pipe } from './pipe';

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

        const flipc = <A, B, C>(f: (a: A, b: B) => C) => (b: B) => (a: A) => f(a, b);
        const flippedAp = flipc(maybeApplicativeInstance.ap)

        expect(
            Pipe(maybeApplicativeInstance.pure((a: number) => (b: number) => a + b))
                (flippedAp(some(1)))
                (flippedAp(some(4))).
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

                    const rv = (a: $<T, [A]>) => {
                        return Pipe(i.ap<A, R>(f, a))
                    }
                    rv.v = f;
                    return rv;
                }

                return Pipe(pureF);
            }
        }

        expect(
            ApplicativePipe(maybeApplicativeInstance)
                ((a: number) => (b: number) => a + b)
                (some(1))
                (some(4)).v
        ).toEqual(
            some(5)
        )
    })

    it("Pipe", () => {
        const plus1 = (n: number) => n + 1;
        const times2 = (n: number) => n * 2;

        expect(
            Pipe(2)(plus1)(times2).v
        ).toEqual(
            6
        )
    })
});

describe('Monad', () => {
    it("Maybe", () => {
        type Maybe<A> = { type: 'none' } | { type: 'some', value: A };
        const none: Maybe<never> = ({ type: 'none' });
        const some = <A>(value: A): Maybe<A> => ({ type: 'some', value });

        const maybeMonadInstance = Monad<Maybe<_>>({
            pure: some,
            fmap: (ta, f) => {
                return ta.type === 'none'
                    ? none
                    : f(ta.value)
            }
        });

        expect(
            maybeMonadInstance.pure(3)
        ).toEqual(
            some(3)
        );

        expect(
            maybeMonadInstance.fmap(some(3), n => some(n + 1))
        ).toEqual(
            some(4)
        );

        expect(
            maybeMonadInstance.map(n => String(n), some(42))
        ).toEqual(
            some("42")
        );

        expect(
            maybeMonadInstance.ap(some((n: number) => n * 2), some(8))
        ).toEqual(
            some(16)
        );
    });

    describe("State", () => {
        type State2<A, S> = (state: S) => [A, S];
        // type State<A> = State2<A, number>;
        type NumberState<A> = (state: number) => [A, number];
        const State = <A>(a: A): NumberState<A> => (state: number) => [a, state];

        const stateMonadInstance = Monad<NumberState<_>>({
            pure: State,
            fmap: (initialStateMonad, mappingFunction) => {
                return initialState => {
                    const [computationReturn, stateReturn] = initialStateMonad(initialState);
                    const newStateMonad = mappingFunction(computationReturn);
                    return newStateMonad(stateReturn);
                }
            }
        });
        const i = stateMonadInstance;

        const readStateNumber: NumberState<number> = s => [s, s];
        const updateState = (f: (s: number) => number): NumberState<undefined> => s => [undefined, f(s)];
        const put = (s: number): NumberState<undefined> => _s => [undefined, s];

        const runState = <A>(s: number, state: NumberState<A>): [A, number] => state(s);


        const createStateMonadInstance = <S>() => {
        }


        const tick: NumberState<undefined> = s => [undefined, s + 1];

        it('state game', () => {
            type GameTuple = [boolean, number];

            type State<A> = (state: GameTuple) => [A, GameTuple]
            const State = <A>(a: A): State<A> => (state: GameTuple) => [a, state];

            const i = Monad<State<_>>({
                pure: State,
                fmap: (initialStateMonad, mappingFunction) => {
                    return initialState => {
                        const [computationReturn, stateReturn] = initialStateMonad(initialState);
                        const newStateMonad = mappingFunction(computationReturn);
                        return newStateMonad(stateReturn);
                    }
                }
            });

            const readState: State<GameTuple> = (s: GameTuple) => [s, s];
            const put = (s: GameTuple): State<undefined> => _ => [undefined, s];
            const runState = <A>(s: GameTuple, state: State<A>): [A, GameTuple] => state(s);

            type Input = 1 | 2 | 3;

            type PlayGame = (input: Input[]) => State<number>;
            const playGame: PlayGame = input => {
                if (input.length === 0) {
                    return i.fmap(
                        readState,
                        ([_, score]) => i.pure(score)
                    )
                } else {
                    const [x, ...rest] = input;
                    return i.fmap(
                        readState,
                        ([on, score]) => {
                            if(on && x === 1) {
                                return i.fmap(
                                    put([on, score + 1]),
                                    _ => playGame(rest)
                                );
                            } else if(on && x === 2) {
                                return i.fmap(
                                    put([on, score - 1]),
                                    _ => playGame(rest)
                                );
                            } else if(x === 3) {
                                return i.fmap(
                                    put([!on, score]),
                                    _ => playGame(rest)
                                )
                            } else {
                                return i.fmap(
                                    put([on, score]),
                                    _ => playGame(rest)
                                )
                            }
                        }
                    )
                }
            }

            expect(
                runState([false, 0], playGame([1, 1, 3, 1, 1, 1, 2, 3, 1, 1, 3, 2]))
            ).toEqual(
                [1, [true, 1]]
            )
        })

        it("pure works", () => {
            expect(
                stateMonadInstance.pure("hi")(5)
            ).toEqual(
                ["hi", 5]
            );
        });

        it("fmap works", () => {
          expect(
              stateMonadInstance.fmap(
                  stateMonadInstance.pure("hi"),
                  initialValue => {
                      return initialState => [initialValue + ", world", initialState + 1]
                  }
              )(6)
          ).toEqual(
              ["hi, world", 7]
          )
        });

        it("helper functions work", () => {
            const s0 = stateMonadInstance.pure("hi");
            const s1 = stateMonadInstance.fmap(s0, _value => readStateNumber);
            const s2 = runState(5, s1);
            expect(s2).toEqual([5, 5]);
        });

        it("tick", () => {
            const s0 = tick;
            const s1 = stateMonadInstance.fmap(s0, _a => State("working"));
            const actual = runState(3, s1);

            expect(actual).toEqual(["working", 4]);
        });

        it("updateState basic", () => {
            const s1 = stateMonadInstance.fmap(tick, _a => updateState(n => n / 2))
            const actual = runState(3, s1);
            expect(actual).toEqual([undefined, 2]);
        });

        it("update state based on value", () => {
            const s1 = stateMonadInstance.fmap(tick, _a => State(4));
            const s2 = stateMonadInstance.fmap(s1, a => updateState(n => n + a));
            const s3 = stateMonadInstance.fmap(s2, _a => readStateNumber);
            const actual = runState(1, s3);
            expect(actual).toEqual([6, 6])
        });

        it("put, pure", () => {
            const process =
                i.fmap(
                    put(9),
                    _a => i.pure("x")
                )
            const actual = runState(4, process);


            // const actual =
            //     Pipe(put(9)).
            //     p(i.then(i.pure("x"))).
            //     p(runStateC(4)).
            //     v;

            expect(actual).toEqual(["x", 9])
        });

        it("postincrement", () => {
            const process =
                i.fmap(
                    readStateNumber,     // first function rv
                    x =>           // add to context
                        i.fmap(
                            put(x + 1), // second function rv
                            _a =>       // add to context
                                i.fmap(
                                    readStateNumber, // third function rv
                                    y =>       // add to context
                                        i.fmap(
                                            put(y + 1), // fourth function rv
                                            _a =>       // add to context
                                                i.return(x) // fifth function rv
                                        )
                                )
                        )
                )


            /*
            const process = monadDo([
                ['x', c => readState],
                ['_', c => put(c.x + 1)],
                ['y', c => readState],
                ['_', c => put(c.y + 1)],
                ['_', c => i.pure(x)]
            ])

            const process = Do()
                ('x', c => readState)
                ('_', c => put(c.x + 1))
                ('y', c => readState)
                ('_', c => put(c.y + 1))
                ('_', c => i.pure(c.x)).v

            */

            /*
            const process = do
                x <- readState;
                a <- put(x + 1);
                y <- readState;
                b <- put(y + 1);
                c <- i.pure(x);
            */


            const actual = runState(1, process);
            expect(actual).toEqual([1, 3])
        })

        it("predecrement", () => {
            const predecrement = i.fmap(
                readStateNumber,
                x =>
                    i.fmap(
                        put(x - 1),
                        _a => readStateNumber
                    )
            );

            /*
            const predecrement = do
                x <- readstate;
                put(x - 1);
                readState;
            */

            const actual = runState(4, predecrement);
            expect(actual).toEqual([3, 3])
        });

        it("write first", () => {
            const writeFirst = i.fmap(
                put(3),
                _a => i.fmap(
                    readStateNumber,
                    x => i.fmap(
                        put(x - 1),
                        _a => i.return(x)
                    )
                )
            )
            expect(runState(42, writeFirst)).toEqual([3, 2])
        })


        const Do1 = <T>(i: Monad<T>) => {
            const $fmapb3 = ($context: any) => {
                const rv: any = ($key3: any, $f3: any) => $f3($context);

                rv.value = null // ???????

                return rv;
            }

            const $return2 = ($context: any) => ($key2: any) => {
                const rv = ($key3: any, $f3: any) => ($value2: any) => $fmapb3({ ...$context, [$key2]: $value2 })($key3, $f3);

                rv.value = ($value2: any) => $fmapb3({ ...$context, [$key2]: $value2 }).value;

                return rv;
            }

            const $fmapb2 = ($context: any) => ($key2: any, $f2: any) => {
                const rv = ($key3: any, $f3: any) =>
                    i.fmap(
                        $f2($context),
                        $return2($context)($key2)($key3, $f3)
                    );

                rv.value =
                    i.fmap(
                        $f2($context),
                        $return2($context)($key2).value
                    );

                return rv;
            }

            const $return1 = ($context: any) => ($key1: any) => ($key2: any, $f2: any) => {
                const rv = ($key3: any, $f3: any) => ($value1: any) => $fmapb2({ ...$context, [$key1]: $value1 })($key2, $f2)($key3, $f3);

                rv.value = ($value1: any) => $fmapb2({ ...$context, [$key1]: $value1 })($key2, $f2).value

                return rv
            }
            const $fmapb1 = ($context: any) => ($key1: any, $f1: any) => ($key2: any, $f2: any) => {
                const rv = ($key3: any, $f3: any) =>
                    i.fmap(
                        $f1($context),
                        $return1($context)($key1)($key2, $f2)($key3, $f3)
                    )

                rv.value =
                    i.fmap(
                        $f1($context),
                        $return1($context)($key1)($key2, $f2).value
                    )

                return rv;
            }

            return $fmapb1({});
        }


        it("basic", () => {
            const process0 = i.fmap(
                readStateNumber,
                x => i.pure(x)
            );

            const process = i.fmap(
                readStateNumber,
                x =>
                    i.fmap(
                        put(x + 1),
                        _a => i.pure(x)
                    )
            );
            expect(runState(42, process)).toEqual([42, 43]);
        })

        it("Do v1", () => {
            const process = Do1(i)
                ('x', (_: any) => readStateNumber)
                ('_', (c: any) => put(c.x + 1))
                ('_', (c: any) => i.pure(c.x))

            expect(runState(42, process)).toEqual([42, 43]);
        })

        it("Do shorter", () => {
            const p0 = i.fmap(
                readStateNumber,
                x => i.fmap(
                    i.return(12),
                    y => i.fmap(
                        put(x + y),
                        _a => i.return(9001)
                    )
                )
            )
            expect(runState(80, p0)).toEqual([9001, 92])

            const p1 = Do(i, [
                { x: (_: any) => readStateNumber },
                { y: (_: any) => i.pure(12) },
                { _: (c: any) => put(c.x + c.y) },
                { _: (_: any) => i.pure(9001) }
            ])
            expect(runState(80, p1)).toEqual([9001, 92]);

            type DoArray<T, A> = Array<{ [key: string]: (c: any) => $<T, [A]> }>;
            const z0: DoArray<NumberState<_>, any> = [{ x: (_: any) => i.pure("hi") }];
        })

        it('V2', () => {
            type AAAA<F> = F extends (a: boolean) => boolean ? (a: number) => number : never;
            const AAA = <F extends (a: any) => any>(f: AAAA<F>) => f
            AAA(n => n)

            type ReverseInner<A extends any[], M extends any[]> = (
                unknown extends Head<A> ? (
                    { [indirect]: M }
                ) : (
                    { [indirect]: ReverseInner<Tail<A>, Cons<Head<A>, M>> }
                )
            )[typeof indirect];
            type Reverse<A extends any[]> = ReverseInner<A, []>

            type Ar<Monad, Z extends any[], M, T, A> = (
                A extends Array<any> ? (
                    // check if the array is empty
                    unknown extends Head<A> ? (
                        { [indirect]: Reverse<Z> }
                    ) : (
                        Head<A> extends { [Key in keyof Head<A>]: (a: M) => $<Monad, [infer R]> } ? (
                            { [indirect]: Ar<Monad, Cons<{ [Key in keyof Head<A>]: (a: M) => NumberState<R> }, Z>, M & { [K2 in keyof Head<A>]: R }, R, Tail<A>> }
                        ) : (
                            { [indirect]: never }
                        )
                    )
                ) : (
                    { [indirect] : never }
                )
            )[typeof indirect];
            type Ar1<A> = Ar<NumberState<_>, [], {}, never, A>
            type Ar2 = Ar1<[{ x: (a: any) => NumberState<number> }, { y: (a: any) => NumberState<number> }, { z: (a: any) => NumberState<undefined> }, { o: (c: any) => NumberState<number> }]>;
            type ZZ<A> = A extends Ar1<A> ? Ar1<A> : never;
            type ZZ1 = ZZ<[{ x: (a: any) => NumberState<number> }, { y: (a: any) => NumberState<number> }, { z: (a: any) => NumberState<undefined> }, { o: (c: any) => NumberState<number> }]>;
            type ZZZ = [{ x: (a: any) => NumberState<number> }] extends ZZ1 ? 1 : 0;
            // const aaaaa: Ar2 = { x: "3", y: false };
            // aaaaa

            type V2<A> = (
                A extends { [key: string]: (c: infer C) => infer U } ? (
                    { [indirect]: { [key: string]: (c: C & { [key in keyof A]: U }) => any } }
                ): (
                    { [indirect]: never }
                )
            )[typeof indirect];

            type VV2 = V2<{ x: (c: {}) => 3 }>;

            (() => {
                type Do = <T, Args extends any[]>(i: Monad<T>, ...args: Args & Args extends Ar1<Args> ? Args : never) => any;

                const Do: Do = (i, ...args) => {
                    const createDoChain = <C>(context: C, args: Array<{ [key: string]: (c: C) => $<typeof i, [any]> }>) => {
                        const [next, ...rest] = args;
                        const key = Object.keys(next)[0];
                        const f = next[key];

                        if (args.length === 1) {
                            return f(context);
                        }

                        return i.fmap(
                            f(context),
                            (nextValue: any): any => createDoChain({ ...context, [key]: nextValue }, rest)
                        );

                    }

                    return createDoChain({}, args);
                };

                type Z = Ar1<[{ x: (c: any) => NumberState<number> }]>;

                const p1 = Do(
                    i,
                    { x: (_: any) => readStateNumber },
                );
                expect(runState(80, p1)).toEqual([9001, 92]);

            })();

            (() => {
                // const fakeDo = <A extends { [key: string]: (c: {}) => any }>(a: A) => <F extends V2<A, { x: number }>>(f: F): V2<F, { x: number, y: number }> => f
                const fakeDo = <A extends { [key: string]: (c: any) => any }, M extends any[]>(a: A, m: M) => {
                    const rv = (f: V2<A>) => fakeDo(f, [...m, a]);
                    rv.value = Do(i, [...m, a]);
                    return rv
                }
                // const tuple = <T extends unknown[]>(...args: T) => args;
                // type ZZ3 = Ar1<typeof a>;
                // TODO: c is always any
                const b = fakeDo({ x: _ => readStateNumber }, [])({ y: c => i.pure(c.x) })({ _: c => put(c.x + c.y) }).value
                expect(runState(4, b)).toEqual([undefined, 8]);
                // const d = c({ z: c => i.put(x + y) })
                // d
            })();

            (() => {
                type ReverseInner<A extends any[], M extends any[]> = (
                    unknown extends Head<A> ? (
                        { [indirect]: M }
                    ) : (
                        { [indirect]: ReverseInner<Tail<A>, Cons<Head<A>, M>> }
                    )
                )[typeof indirect];
                type Reverse<A extends any[]> = ReverseInner<A, []>

                type V2<T, A> = (
                    A extends { [key: string]: (c: infer C) => $<T, [infer U]> } ? (
                        { [indirect]: { [key: string]: (c: C & { [key in keyof A]: U }) => any } }
                    ): (
                        { [indirect]: never }
                    )
                )[typeof indirect];

                const blah = <M extends any[]>(...m: M) => {
                    type Z = M["length"] extends 0 ? 1 : 0;
                    return null as unknown as Z;
                }
                const b = blah(1);

                const emptyTuple = (<M extends any[]>(...m: M) => m)()

                const fakeDo = <T>(_: Monad<T>) => {
                    const inner = <A extends { [key: string]: (c: any) => any }, M extends any[] & { length: number }>(a: A, m: M) => {
                        const rv = <B extends V2<T, A>>(b: B) => inner(b, [a, ...m] as Cons<A, M>);
                        rv.monad = () => {
                            const rv = [a, ...m] as Cons<A, M>;
                            type R = Reverse<typeof rv>;
                            return rv.reverse() as unknown as R;
                        }
                        return rv;
                    }

                    return <A extends { [key: string]: (c: {}) => any }>(a: A) => {
                        return inner(a, emptyTuple);
                    }
                }

                // const a = fakeDo({ x: _ => i.pure("hi") })({ y: _ => readStateNumber })({ z: c => i.pure(c.x) }).value;
                const a = fakeDo(i)({ x: _ => i.pure("hi") })({ y: _ => readStateNumber })({ z: c => i.pure(c.x) }).monad();
                a

                // type LastReturnValue<I extends Array<{ [key: string]: (c: any) => any }>> = {
                //     0: Head<I>[keyof Head<I>] extends (c: any) => infer R ? R : never;
                //     1: LastReturnValue<Tail<I>>;
                // }[unknown extends Head<Tail<I>> ? 0 : 1]
                // interface LastReturnValueInterface<I extends Array<{ [key: string]: (c: any) => any }>> {
                //     0: Head<I>[keyof Head<I>] extends (c: any) => infer R ? R : never;
                //     1: LastReturnValueInterface<Tail<I>>[unknown extends Head<Tail<I>> ? 0 : 1];
                // }
                // type LastReturnValue<I extends Array<{ [key: string]: (c: any) => any }>> = LastReturnValueInterface<I>[unknown extends Head<Tail<I>> ? 0 : 1]
                type LastReturnValue<I extends any[]> = Last<I> extends { [key: string]: (c: any) => infer R } ? R : never;

                const head = <A extends any[]>(...args: A) => args[0] as Head<A>;
                const tail = <A extends any[]>(...args: A) => args.splice(1) as Tail<A>;
                const objectKey = <A extends {}>(obj: A) => Object.keys(obj)[0] as keyof A;

                const Do = <T, Args extends Array<{ [key: string]: (c: any) => $<T, [any]> }>>(i: Monad<T>, args: Args) => {
                    const createDoChain = <C, Args extends Array<{ [key: string]: (c: C) => $<T, [any]> }>>(context: C, args: Args): LastReturnValue<Args> => {
                        const next = head(...args);
                        const rest = tail(...args);
                        const key = objectKey(next);
                        const f = next[key];

                        type NextValue = typeof f extends (c: C) => $<T, [infer R]> ? R : never;

                        if (rest.length === 0) {
                            return f(context) as LastReturnValue<Args>;
                        }

                        return i.fmap(
                            f(context),
                            (nextValue: NextValue): LastReturnValue<typeof rest> => {
                                const newContext = { ...context, [key]: nextValue } as C & { [k in typeof key]: NextValue }

                                return createDoChain(newContext, rest)
                            }
                        );

                    }

                    return createDoChain({}, args);
                };

                const p0 =
                    fakeDo(i)
                    ({ x: _ => readStateNumber })
                    ({ y: _ => i.pure(12) })
                    ({ _: c => put(c.x + c.y) })
                    ({ _: _ => i.pure(9001) })
                    .monad();

                const p00 = Do(i, p0);
                expect(runState(80, p00)).toEqual([9001, 92]);


                const p1 = Do(i, [
                    { x: (_: any) => readStateNumber },
                    { y: (_: any) => i.pure(12) },
                    { _: (c: any) => put(c.x + c.y) },
                    { _: (_: any) => i.pure(9001) }
                ]);
                expect(runState(80, p1)).toEqual([9001, 92]);
            })()


            const tuple = <M extends unknown[]>(...m: M): M => m
            const t = 1;
            const u = tuple(...[1, 2, "string"]);
            const q = [t, ...u];

            (() => {
                type Rec<A, M extends any[], L> = (
                    M["length"] extends L ? (
                        { [indirect]: M }
                    ) : (
                        { [indirect]: Rec<A, Cons<A, M>, L> }
                    )
                )[typeof indirect];

                const recursive = <A, M extends any[]>(q: M extends Rec<A, [], M["length"]> ? true : false, a: A, ...m: M) => {
                    return [a, ...m] as Cons<A, M>
                }
                expect(
                    recursive(true, 3)
                ).toEqual([3]);
                expect(
                    recursive(true, 3, 4)
                ).toEqual([3, 4])
                expect(
                    recursive(false, 3, 4, "hi")
                ).toEqual([3, 4, "hi"])
                expect(
                    recursive(true, 3, 4, 5, 6)
                ).toEqual([3, 4, 5, 6])
            })()

            type Cycle<A> = A extends number ? (
                string
            ) : (
                A extends string ? (
                    boolean
                ) : (
                    A extends boolean ? (
                        number
                    ) : (
                        never
                    )
                )
            )
            const cycle = <A>(a: A) => {
                const rv = (b: Cycle<A>) => cycle(b);
                rv.value = a;
                return rv;
            }
            const a = cycle("hi")(true)(3).value
            expect(a).toEqual(3);

            // const fakeDo = <F>(f: Ar1<F>) => f
            // fakeDo([
            //     { x: c => 3 },
            //     { y: c => true }
            // ])

            // const fakeDo = <A, F extends Ar1<A>>(f: A): F => f
            // fakeDo([
            //     { x: c => 3 },
            //     { y: c => true }
            // ])
        });

        it('V1', () => {
            type Ar<M, T, A> = (
                A extends Array<any> ? (
                    // check if the array is empty
                    unknown extends Head<A> ? (
                        { [indirect]: M }
                    ) : (
                        Head<A> extends { [Key in keyof Head<A>]: (a: T extends never ? any : T) => infer R } ? (
                            { [indirect]: Ar<M & { [K2 in keyof Head<A>]: R }, R, Tail<A>> }
                        ) : (
                            { [indirect]: never }
                        )
                    )
                ) : (
                    { [indirect] : never }
                )
            )[typeof indirect];
            type Ar1<A> = Ar<{}, never, A>
            type Ar2 = Ar1<[{ x: (a: number) => string }, { y: (a: string) => boolean }]>;
            const aaaaa: Ar2 = { x: "3", y: false };
            aaaaa
        });
    });
});

declare const indirect: unique symbol;
type Head<L extends any[]> = ((...l: L) => void) extends ((h: infer H, ...t: any) => void) ? H : never;
type Tail<L extends any[]> = ((...l: L) => void) extends ((h: any, ...t: infer T) => void) ? T : never
type Prev<T extends number> = [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62][T];
type Last<L extends any[]> = L[Prev<L extends { length: infer Length } ? Length : never>];
type Cons<H, T extends any[]> = ((h: H, ...t: T) => void) extends ((...c: infer C) => void) ? C : never;

const Do = <T>(i: Monad<T>, args: Array<{ [key: string]: (c: any) => $<T, [any]> }>) => {
    const createDoChain = <C>(context: C, args: Array<{ [key: string]: (c: C) => $<T, [any]> }>) => {
        const [next, ...rest] = args;
        const key = Object.keys(next)[0];
        const f = next[key];

        if (args.length === 1) {
            return f(context);
        }

        return i.fmap(
            f(context),
            (nextValue: any): any => createDoChain({ ...context, [key]: nextValue }, rest)
        );

    }

    return createDoChain({}, args);
};

// describe.only("demo", () => {
//     it("demos", () => {
//         let v = 3;
//         v = 4;
//         v = 5;
//         v = 6;

//         const add1 = (a: number) => a + 1;
//         expect(add1(2)).toEqual(3);

//         const doSomething = (f: (a: number) => number) => {
//             return f(1);
//         }

//         const doSomething2 = (f: (a: number) => number, v: number) => {
//             return f(v);
//         }

//         const genericSOmething = <A>(f: (a: A) => A, v: A) => {
//             return f(v);
//         }

//         a = [1];

//         const mapArray = <A>(f: (a: A) => A, v: Array<A>) => {
//             const arrayValue = v[0];
//             return [f(arrayValue)]
//         }

//         const mappArrayCool <A>(f: (a: A) => A, v: Array<A>) => {
//             const newv = []
//             for(let i = 0; i < v.length; i++ ) {
//                 newv[i] = f(v[i]);
//             }
//             return newv
//         }


//         const mapObj = <A>(f: (a: A) => A, container: { v: A }) => {
//             const inner = container.v;
//             return { v: f(inner) }
//         }

//         type Obj<A> = { v: A }
//         const obj: Obj<number> = { v: 2 }
//         interface Functor<T> {
//             map: <A, B>(f: (a: A) => B, v: $<T, [A]>) => $<T, [B]>;
//         }
//         const Functor = <T>(spec: Functor<T>): Functor<T> => spec;

//         const arrayFunctorInstance = Functor<Array<_>>({
//             map: (f, array) => {
//                 const v = array[0];
//                 return [f(v)]
//             }
//         });
//         arrayFunctorInstance.map(n => String(n), [1]);

//         const objFunctorInstance = Functor<Obj<_>>({
//             map: (f, obj) => {
//                 return { v: f(obj.v) }
//             }
//         });





//         interface Applicative<T> extends Functor<T> {
//             pure: <A>(a: A) => $<T, [A]>;

//             // map: <A, B>(f: (a: A) => B, v: $<T, [A]>) => $<T, [B]>;
//             ap: <A, B>(f: $<T, [(a: A) => B]>, v: $<T, [A]>) => $<T, [B]>;
//         }
//     })
// });
