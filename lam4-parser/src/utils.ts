/** Branded types 
 * https://egghead.io/blog/using-branded-types-in-typescript
 *  See also the TS playground excerpt linked from the article above
 */
declare const __brand: unique symbol;
type Brand<B> = { [__brand]: B };
export type Branded<T, B> = T & Brand<B>;


export type JSONString = Branded<string, "JSON">;