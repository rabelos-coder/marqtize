import { AuthLogin, LoginInput, Register, RegisterInput } from "@/types/auth";
import { TypedDocumentNode, gql } from "@apollo/client";

const USER_PROPS = gql`
  fragment UserProps on User {
    id
    name
    systemName
    email
    image
    isActive
    isSuperAdmin
    language
    timezoneId
    type
    claims
    timezone {
      id
      code
      name
    }
    customer {
      id
      corporateName
      tradingName
      systemName
      slug
    }
  }
`;

export const AUTH_LOGIN: TypedDocumentNode<AuthLogin, LoginInput> = gql`
  ${USER_PROPS}
  mutation AuthLogin($data: LoginInput!) {
    authLogin(data: $data) {
      token
      user {
        ...UserProps
      }
    }
  }
`;

export const REGISTER: TypedDocumentNode<Register, RegisterInput> = gql`
  ${USER_PROPS}
  mutation Register($data: RegisterInput!) {
    register(data: $data) {
      ...UserProps
    }
  }
`;
