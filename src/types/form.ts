import { ChangeEvent } from 'react'

export type StepperHorizontalPropsType = {
  mode: 'create' | 'update' | 'view'
  level: number
  setLevel: (value: number) => void
  steps: string[]
  disabled: boolean
}

type BasicInputFormValueInterface = {
  email: string
  firstName: string
  password: string
  confirmPassword: string
  agreeTerms: boolean
  placeHolderName: string
  cardNumber: string
  expiration: string
  cvvNumber: string
  uploadDocumentation: string
  informationCheckBox: boolean
  linkedInLink: string
  gitHubLink: string
  giveFeedBack: string
  state: string
  agreeConditions: boolean
}

export type NumberingWizardPropsType = {
  getUserData: (event: ChangeEvent<HTMLInputElement>) => void
  basicInputFormValue: BasicInputFormValueInterface
  level?: number
}

export type CommonPropsTypes = {
  callbackActive: (val: number | undefined) => void
}

export type NewAddressModalPropsTypes = {
  toggle: () => void
  showModal: boolean
}

export type PaymentMethodOptionPropsType = {
  paymentMethodName: string
  getUserData: (event: ChangeEvent<HTMLInputElement>) => void
}

type RadioBoxValuesInterFace = {
  address: string
  shippingMethod: string
}

export type ShippingInformationCommonProps = {
  handleNextButton?: () => void
  radioBoxValues: RadioBoxValuesInterFace
  getUserData: (event: ChangeEvent<HTMLInputElement>) => void
}

export type ShippingFormTabContentPropsType = {
  activeTab: number | undefined
  callbackActive: (val: number | undefined) => void
}

type StudentValidationFormInterFace = {
  password: string
  name: string
  email: string
  confirmPassWord: string
  portfolioURL: string
  projectDescription: string
  twitterUrl: string
  gitHubUrl: string
}

export type StudentFormPropsType = {
  handleImageLabelClick: () => void
  imageUrl: string | null
  getUserData: (event: ChangeEvent<HTMLInputElement>) => void
  studentValidationForm: StudentValidationFormInterFace
  level: number
  handleNextButton: () => void
  fileInputRef: any
  handleBackButton: () => void
}

export type VerticalValidationWizardFormPropsType = {
  activeCallBack: (val: number) => void
  activeTab?: number
}

export type VerticalFormPropsType = {
  callbackActive: (val: number) => void
  activeTab: number
}
