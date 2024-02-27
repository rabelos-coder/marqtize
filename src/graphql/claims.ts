import { gql, TypedDocumentNode } from "@apollo/client";

import { CountClaim, FindFirstClaim, FindManyClaim } from "@/types/claim";
import { WhereInput } from "@/types/common";

const FRAGMENT_CLAIM_PROPS = gql`
  fragment ClaimProps on Claim {
    action
    subject
  }
`;

export const COUNT_CLAIMS: TypedDocumentNode<CountClaim, WhereInput> = gql`
  query CountClaim($where: SearchClaimInput) {
    countClaim(where: $where)
  }
`;

export const FIND_MANY_CLAIMS: TypedDocumentNode<FindManyClaim, WhereInput> =
  gql`
    ${FRAGMENT_CLAIM_PROPS}
    query FindManyClaim($where: SearchClaimInput) {
      findManyClaim(where: $where) {
        ...ClaimProps
      }
    }
  `;

export const FIND_FIRST_CLAIM: TypedDocumentNode<FindFirstClaim, WhereInput> =
  gql`
    ${FRAGMENT_CLAIM_PROPS}
    query FindFirstClaim($where: SearchClaimInput) {
      findFirstClaim(where: $where) {
        ...ClaimProps
      }
    }
  `;
