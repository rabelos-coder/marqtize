export type JWT = {
  id: string;
  tokenId: string | null;
  sa: boolean;
  systemName: string;
  customer: object | null;
  email: string;
  roles: string[];
  claims: string[];
  type: "APPLICATION" | "VIRTUAL" | "SOCIAL";
  lang: "en" | "pt";
  image: string | null;
  hdGroups: string[];
  hdDepartments: string[];
  iat: number;
  exp: number;
};
