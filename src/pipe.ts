export interface Pipe<A> {
    v: A,
    <B>(f: (a: A) => B): Pipe<B>;
}
export const Pipe = <A>(a: A): Pipe<A> => {
    const rv: Pipe<A> = f => Pipe(f(a));
    rv.v = a;
    return rv;
}
