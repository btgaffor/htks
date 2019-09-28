import { $ } from '../';
import { Applicative } from '../mine'

export interface Monad<T> extends Applicative<T> {
    bind: <A, B, X>(ta: $<T, [A, X]>, f: (x: A) => $<T, [B, X]>) => $<T, [B, X]>;
}
export const Monad = <T>({ pure, bind }: Pick<Monad<T>, 'pure' | 'bind'>): Monad<T> => ({
    bind,
    ...Applicative({
        pure,
        ap: (tf, ta) => bind(tf, f => bind(ta, x => pure(f(x))))
    })
})

// export const map2 = <T>(i: Monad<T>) => <A, B, C D, E>(f: (a: D, b: D) => $<T, [C]>, ta: (a: A) => $<T, [B]>, tb: (c: C) => $<T, [D]>): $<T, [E]> => {
    
// }

/*
const process = i.fmapb(
    readState,
    x => i.fmapb(
        put(x + 1),
        _a => i.fmapb(
            readState,
            y => i.fmapb(
                put(y + 1),
                _a =>
                    i.pure(x)
            )
        )
    )
)
*/

/*
const process = do
    x <- readState;
    _ <- put(x + 1);
    y <- readState;
    _ <- put(y + 1);
    _ <- i.pure(x);
*/

/*
const process = Do(stateMonadInstance)
    ('x', c => readState)
    ('_', c => put(c.x + 1))
    ('y', c => readState)
    ('_', c => put(c.y + 1))
    ('_', c => i.pure(c.x)).v
*/

/*
const process = Do(stateMonadInstance)
    ('_', c => put(3))
    ('_', c => i.pure(42))
*/


const Do1 = <T>(i: Monad<T>) => {
    const context: any = {};

    const pipe = (key: any, f: any) => {
        const pa = f(context);

        return (key2: any, f2: any) => {
            return i.bind(
                pa,
                (nextValue: any) => {
                    context[key] = nextValue
                    return f2(context)
                }
            )
        }
    }

    return pipe;
};

const Do2 = <T>(i: Monad<T>) => {
    const context: any = {};

    const pipe = (key1: any, f1: any) => {
        return (key2: any, f2: any) => {
            return (key3: any, f3: any) => {

                return i.bind(
                    // context[key0] = value1
                    f1(context),
                    (value1: any) => {
                        context[key1] = value1
                        return i.bind(
                            f2(context),
                            (value2: any) => {
                                context[key2] = value2;
                                return f3(context);
                            }
                        )
                    }
                )

            }
        }
    }

    return pipe;
}

export const Do = <T>(i: Monad<T>, args: Array<{ [key: string]: any }>) => {
    const createDoChain = (context: any, args: Array<{ [key: string]: any }>) => {
        const [next, ...rest] = args;
        const key = Object.keys(next)[0];
        const f = next[key];

        if (args.length === 1) {
            return f(context);
        }

        return i.bind(
            f(context),
            (nextValue: any): any => createDoChain({ ...context, [key]: nextValue }, rest)
        );

    }

    return createDoChain({}, args);
};
