throw new Error("boom, I caused a conflict");

// must have an export or import, otherwise closure won't detect it as es6
export {b: 1};
