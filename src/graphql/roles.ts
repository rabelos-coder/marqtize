import { gql, TypedDocumentNode } from '@apollo/client'

import {
  FindByIdInput,
  FindByIdsInput,
  PaginatedInput,
  WhereAndOrderInput,
  WhereInput,
} from '@/types/common'
import {
  CountRole,
  CreateRole,
  CreateRoleInput,
  DeleteManyRole,
  DeleteRole,
  FindByIdRole,
  FindFirstRole,
  FindManyRole,
  PaginatedRole,
  RemoveManyRole,
  RemoveRole,
  RestoreManyRole,
  RestoreRole,
  UpdateRole,
  UpdateRoleInput,
} from '@/types/role'

const FRAGMENT_ROLE_PROPS = gql`
  fragment RoleProps on Role {
    id
    accountId
    name
    slug
    claims
    isDeleteable
    isDefault
    createdAt
    updatedAt
    deletedAt
    users {
      id
      name
      systemName
    }
    account {
      id
      corporateName
      tradingName
      systemName
      slug
    }
  }
`

export const COUNT_ROLES: TypedDocumentNode<CountRole, WhereInput> = gql`
  query CountRole($where: SearchRoleInput) {
    countRole(where: $where)
  }
`

export const FIND_MANY_ROLES: TypedDocumentNode<
  FindManyRole,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_ROLE_PROPS}
  query FindManyRole($where: SearchRoleInput, $orderBy: SortRoleInput) {
    findManyRole(where: $where, orderBy: $orderBy) {
      ...RoleProps
    }
  }
`

export const PAGINATED_ROLES: TypedDocumentNode<PaginatedRole, PaginatedInput> =
  gql`
    ${FRAGMENT_ROLE_PROPS}
    query PaginatedRole(
      $page: Int!
      $perPage: Int!
      $where: SearchRoleInput
      $orderBy: SortRoleInput
    ) {
      paginatedRole(
        page: $page
        perPage: $perPage
        where: $where
        orderBy: $orderBy
      ) {
        data {
          ...RoleProps
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
  `

export const FIND_ROLE: TypedDocumentNode<FindByIdRole, FindByIdInput> = gql`
  ${FRAGMENT_ROLE_PROPS}
  query FindByIdRole($id: String!) {
    findByIdRole(id: $id) {
      ...RoleProps
    }
  }
`

export const FIND_FIRST_ROLE: TypedDocumentNode<
  FindFirstRole,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_ROLE_PROPS}
  query FindFirstRole($where: SearchRoleInput, $orderBy: SortRoleInput) {
    findFirstRole(where: $where, orderBy: $orderBy) {
      ...RoleProps
    }
  }
`

export const CREATE_ROLE: TypedDocumentNode<CreateRole, CreateRoleInput> = gql`
  ${FRAGMENT_ROLE_PROPS}
  mutation CreateRole($data: CreateRoleInput!) {
    createRole(data: $data) {
      ...RoleProps
    }
  }
`

export const UPDATE_ROLE: TypedDocumentNode<UpdateRole, UpdateRoleInput> = gql`
  ${FRAGMENT_ROLE_PROPS}
  mutation UpdateRole($data: UpdateRoleInput!) {
    updateRole(data: $data) {
      ...RoleProps
    }
  }
`

export const DELETE_ROLE: TypedDocumentNode<DeleteRole, FindByIdInput> = gql`
  mutation DeleteRole($id: String!) {
    deleteRole(id: $id)
  }
`

export const DELETE_ROLES: TypedDocumentNode<DeleteManyRole, FindByIdsInput> =
  gql`
    mutation DeleteManyRole($ids: [String!]!) {
      deleteManyRole(ids: $ids)
    }
  `

export const REMOVE_ROLE: TypedDocumentNode<RemoveRole, FindByIdInput> = gql`
  mutation RemoveRole($id: String!) {
    removeRole(id: $id)
  }
`

export const REMOVE_ROLES: TypedDocumentNode<RemoveManyRole, FindByIdsInput> =
  gql`
    mutation RemoveManyRole($ids: [String!]!) {
      removeManyRole(ids: $ids)
    }
  `

export const RESTORE_ROLE: TypedDocumentNode<RestoreRole, FindByIdInput> = gql`
  mutation RestoreRole($id: String!) {
    restoreRole(id: $id)
  }
`

export const RESTORE_ROLES: TypedDocumentNode<RestoreManyRole, FindByIdsInput> =
  gql`
    mutation RestoreManyRole($ids: [String!]!) {
      restoreManyRole(ids: $ids)
    }
  `
