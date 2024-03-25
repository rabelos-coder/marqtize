export type Claim = {
  action: string
  subject: string
}

export type CountClaim = {
  countClaim: number
}

export type FindManyClaim = {
  findManyClaim: Claim[]
}

export type FindFirstClaim = {
  findFirstClaim: Claim
}
