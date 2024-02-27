import { LanguageDataType, ProfileType } from "./layout";

export const CountryClassName: Record<string, string> = {
  en: "us",
  pt: "pt",
};

export const Languages: LanguageDataType[] = [
  {
    name: "English",
    shortName: "en",
    iconClass: "flag-icon flag-icon-us",
    tag: "(US)",
  },
  {
    name: "Português",
    shortName: "pt",
    iconClass: "flag-icon flag-icon-pt",
    tag: "(BR)",
  },
];

export const ProfileListData: ProfileType[] = [
  {
    icon: "User",
    text: "Account",
    path: "/app/users/profile",
  },
];
