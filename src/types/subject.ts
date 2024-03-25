export const PublicSubjects = {
  All: 'All',
  User: 'User',
  Role: 'Role',
}

export const ProtectedSubjects = {
  Claim: 'Claim',
}

export const Subjects = {
  ...PublicSubjects,
  ...ProtectedSubjects,
}

export type Subject = keyof typeof Subjects
