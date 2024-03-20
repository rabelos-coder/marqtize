export const PublicSubjectEnum = {
  All: 'All',
  User: 'User',
  Role: 'Role',
  Claim: 'Claim',
}

export const ProtectedSubjectEnum = {
  Claim: 'Claim',
}

export const AllSubjectsEnum = {
  ...ProtectedSubjectEnum,
  ...PublicSubjectEnum,
}

export type Subject = keyof typeof AllSubjectsEnum
