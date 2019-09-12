import { $ } from '.';

export interface Monad<T> extends Applicative<T> {
    fmap: <A, B>(f: (x: A) => $<T, [B]>, ta: $<T, [A]>) => $<T, [B]>;
    fmapb: <A, B>(ta: $<T, [A]>, f: (x: A) => $<T, [B]>) => $<T, [B]>;
    fmapc: <A, B>(f: (x: A) => $<T, [B]>) => (ta: $<T, [A]>) => $<T, [B]>;
    then: <B>(b: $<T, [B]>) => <A>(a: A) => $<T, [B]>;
    thenb: <A, B>(a: A, b: $<T, [B]>) => $<T, [B]>;
}
export const Monad = <T>({ pure, fmap }: Pick<Monad<T>, 'pure' | 'fmap'>): Monad<T> => ({
    pure,
    fmap,
    fmapc: f => t => fmap(f, t),
    fmapb: (t, f) => fmap(f, t),
    then: tb => s => fmap(_ta => tb, s),
    thenb: (s, tb) => fmap(_ta => tb, s),
    map: (f, ta) => fmap(x => pure(f(x)), ta),
    mapc: f => ta => fmap(x => pure(f(x)), ta),
    ap: (tf, ta) => fmap(f => fmap(x => pure(f(x)), ta), tf),
    apc: tf => ta => fmap(f => fmap(x => pure(f(x)), ta), tf)
})

