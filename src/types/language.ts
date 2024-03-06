import { LanguageDataType } from "./layout";

export const CountryClassName: Record<string, string> = {
  en: "us",
  pt: "pt",
};

export const LanguagesData: LanguageDataType[] = [
  {
    name: "english",
    shortName: "en",
    iconClass: "flag-icon flag-icon-us",
    tag: "(US)",
  },
  {
    name: "portuguese",
    shortName: "pt",
    iconClass: "flag-icon flag-icon-br",
    tag: "(BR)",
  },
];

export enum LanguageEnum {
  en = "en",
  pt = "pt",
}

export type Language = keyof typeof LanguageEnum;
