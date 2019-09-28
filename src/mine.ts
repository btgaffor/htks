import { $ } from '.';

export interface Functor<T> {
    map: <A, B, X>(f: (x: A) => B, t: $<T, [A, X]>) => $<T, [B, X]>;
    // mapc: <A, B>(f: (x: A) => B) => (t: $<T, [A]>) => $<T, [B]>;
}
export const Functor = <T>({ map }: Pick<Functor<T>, 'map'>): Functor<T> => ({
    map,
    // mapc: f => a => map(f, a)
});

export interface Applicative<T> extends Functor<T> {
    pure: <A, X>(a: A) => $<T, [A, X]>;
    ap: <A, B, X>(tf: $<T, [(a: A) => B, X]>, ta: $<T, [A, X]>) => $<T, [B, X]>;
    apc: <A, B, X>(tf: $<T, [(a: A) => B, X]>) => (ta: $<T, [A, X]>) => $<T, [B, X]>;
}
export const Applicative = <T>({ pure, ap }: Pick<Applicative<T>, 'pure' | 'ap'>): Applicative<T> => ({
    pure,
    ap,
    apc: f => a => ap(f, a),
    ...Functor({
        map: (f, a) => ap(pure(f), a),
    })
});
