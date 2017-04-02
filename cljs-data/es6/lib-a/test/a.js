import { assoc } from "goog:cljs.core";

function foo(x) {
  return assoc(x, "foo", "foo");
}

export { foo };