import { LanguageDataType } from './layout'

export const CountryClassName: Record<string, string> = {
  en: 'us',
  pt: 'pt',
}

export const LanguagesData: LanguageDataType[] = [
  {
    name: 'english',
    shortName: 'en',
    iconClass: 'flag-icon flag-icon-us',
    tag: '(US)',
  },
  {
    name: 'portuguese',
    shortName: 'pt-br',
    iconClass: 'flag-icon flag-icon-br',
    tag: '(BR)',
  },
]

export enum LanguageEnum {
  en = 'en',
  pt_BR = 'pt-br',
}

export type Language = keyof typeof LanguageEnum
