export interface UpdateInlineEventDetails {
  newElement: ChildNode;
}
export type UpdateInlineEvent = CustomEvent<UpdateInlineEventDetails>;

interface RustChargeEventMap {
  "rust-charge:update-inline": UpdateInlineEvent;
}

declare global {
  // eslint-disable-next-line @typescript-eslint/no-empty-interface
  interface ElementEventMap extends RustChargeEventMap {}
}
