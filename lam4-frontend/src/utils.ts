/** Branded types 
 * https://egghead.io/blog/using-branded-types-in-typescript
 *  See also the TS playground excerpt linked from the article above
 */
declare const __brand: unique symbol;
type Brand<B> = { [__brand]: B };
export type Branded<T, B> = T & Brand<B>;


export type JSONString = Branded<string, "JSON">;

export const zip = <A, B>(a: A[], b: B[]) => a.map((k, i) => [k, b[i]]);

/** Either type, adapted from https://gist.github.com/sallar/b66467428a9015711509f70f40e4d233 */

export type Failure<T> = {tag: 'failure'; value: T}
export type Success<T> = {tag: 'success'; value: T}
export type Either<L, R> = Failure<L> | Success<R>; 

export const matchOnEither = <T, L, R>(
  input: Either<L, R>,
  left: (left: L) => T,
  right: (right: R) => T,
) => {
  switch (input.tag) {
    case 'failure':
      return left(input.value);
    case 'success':
      return right(input.value);
  }
};

export const isSuccess = <L, R>(input: Either<L, R>): input is Success<R> => {
  return input.tag === 'success';
};

export const isFailure = <L, R>(input: Either<L, R>): input is Failure<L> => {
  return input.tag === 'failure';
};

export const Success = <T>(value: T): Success<T> => {
  return {
    tag: 'success',
    value,
  };
};

export const Failure = <T>(value: T): Failure<T> => {
  return {
    tag: 'failure',
    value,
  };
};