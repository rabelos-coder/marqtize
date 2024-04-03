export enum PublicSubjectsEnum {
  All = 'All',
  User = 'User',
  Role = 'Role',
}

export enum ProtectedSubjectsEnum {
  Account = 'Account',
  Claim = 'Claim',
  Logging = 'Logging',
}

export const Subjects = {
  ...PublicSubjectsEnum,
  ...ProtectedSubjectsEnum,
}

export type Subject = keyof typeof Subjects
