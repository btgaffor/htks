import { $ } from '.';

export interface Functor<T> {
    map: <A, B>(f: (x: A) => B, t: $<T, [A]>) => $<T, [B]>;
    mapc: <A, B>(f: (x: A) => B) => (t: $<T, [A]>) => $<T, [B]>;
}
export const Functor = <T>({ map }: Pick<Functor<T>, 'map'>): Functor<T> => ({
    map,
    mapc: f => a => map(f, a)
});

export interface Applicative<T> extends Functor<T> {
    pure: <A>(a: A) => $<T, [A]>;
    ap: <A, B>(tf: $<T, [(a: A) => B]>, ta: $<T, [A]>) => $<T, [B]>;
    apc: <A, B>(tf: $<T, [(a: A) => B]>) => (ta: $<T, [A]>) => $<T, [B]>;
}
export const Applicative = <T>({ pure, ap }: Pick<Applicative<T>, 'pure' | 'ap'>): Applicative<T> => ({
    pure,
    ap,
    apc: f => a => ap(f, a),
    ...Functor({
        map: (f, a) => ap(pure(f), a),
    })
});
