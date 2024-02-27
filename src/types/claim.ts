import { Action } from "./action";

export type Claim = {
  action: Action;
  subject: string;
};

export type CountClaim = {
  countClaim: number;
};

export type FindManyClaim = {
  findManyClaim: Claim[];
};

export type FindFirstClaim = {
  findFirstClaim: Claim;
};
