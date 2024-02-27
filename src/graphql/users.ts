import { gql, TypedDocumentNode } from "@apollo/client";

import {
  FindByIdInput,
  FindByIdsInput,
  PaginatedInput,
  PaginatedObject,
  WhereAndOrderInput,
  WhereInput,
} from "@/types/common";
import {
  CountUser,
  CreateUser,
  CreateUserInput,
  DeleteManyUser,
  DeleteUser,
  FindByIdUser,
  FindFirstUser,
  FindManyUser,
  RemoveManyUser,
  RemoveUser,
  RestoreManyUser,
  RestoreUser,
  UpdateUser,
  UpdateUserInput,
  User,
} from "@/types/user";

export const FRAGMENT_USER_PROPS = gql`
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

export const COUNT_USERS: TypedDocumentNode<CountUser, WhereInput> = gql`
  query CountUser($where: SearchUserInput) {
    countUser(where: $where)
  }
`;

export const FIND_USERS: TypedDocumentNode<FindManyUser, WhereAndOrderInput> =
  gql`
    ${FRAGMENT_USER_PROPS}
    query FindManyUser($where: SearchUserInput, $orderBy: SortUserInput) {
      findManyUser(where: $where, orderBy: $orderBy) {
        ...UserProps
      }
    }
  `;

export const PAGINATED_USERS: TypedDocumentNode<
  PaginatedObject<User>,
  PaginatedInput
> = gql`
  ${FRAGMENT_USER_PROPS}
  query PaginatedUser(
    $page: Int!
    $perPage: Int!
    $where: SearchUserInput
    $orderBy: SortUserInput
  ) {
    paginatedUser(
      page: $page
      perPage: $perPage
      where: $where
      orderBy: $orderBy
    ) {
      data {
        ...UserProps
      }
      meta {
        total
        lastPage
        currentPage
        perPage
        prev
        next
      }
    }
  }
`;

export const FIND_USER: TypedDocumentNode<FindByIdUser, FindByIdInput> = gql`
  ${FRAGMENT_USER_PROPS}
  query FindByIdUser($id: String!) {
    findByIdUser(id: $id) {
      ...UserProps
    }
  }
`;

export const FIND_FIRST_USER: TypedDocumentNode<FindFirstUser, WhereInput> =
  gql`
    ${FRAGMENT_USER_PROPS}
    query FindFirstUser($where: SearchUserInput, $orderBy: SortUserInput) {
      findFirstUser(where: $where, orderBy: $orderBy) {
        ...UserProps
      }
    }
  `;

export const CREATE_USER: TypedDocumentNode<CreateUser, CreateUserInput> = gql`
  mutation CreateUser($data: CreateUserInput!) {
    createUser(data: $data) {
      id
    }
  }
`;

export const UPDATE_USER: TypedDocumentNode<UpdateUser, UpdateUserInput> = gql`
  mutation UpdateUser($data: UpdateUserInput!) {
    updateUser(data: $data) {
      id
    }
  }
`;

export const DELETE_USER: TypedDocumentNode<DeleteUser, FindByIdInput> = gql`
  mutation DeleteUser($id: String!) {
    deleteUser(id: $id)
  }
`;

export const DELETE_USERS: TypedDocumentNode<DeleteManyUser, FindByIdsInput> =
  gql`
    mutation DeleteManyUser($ids: [String!]!) {
      deleteManyUser(ids: $ids)
    }
  `;

export const REMOVE_USER: TypedDocumentNode<RemoveUser, FindByIdInput> = gql`
  mutation RemoveUser($id: String!) {
    removeUser(id: $id)
  }
`;

export const REMOVE_USERS: TypedDocumentNode<RemoveManyUser, FindByIdsInput> =
  gql`
    mutation RemoveManyUser($ids: [String!]!) {
      removeManyUser(ids: $ids)
    }
  `;

export const RESTORE_USER: TypedDocumentNode<RestoreUser, FindByIdInput> = gql`
  mutation RestoreUser($id: String!) {
    restoreUser(id: $id)
  }
`;

export const RESTORE_USERS: TypedDocumentNode<RestoreManyUser, FindByIdsInput> =
  gql`
    mutation RestoreManyUser($ids: [String!]!) {
      restoreManyUser(ids: $ids)
    }
  `;
