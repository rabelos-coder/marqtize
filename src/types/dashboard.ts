import { ApexOptions } from 'apexcharts'

export type ChartOptionType = {
  series: ApexOptions['series']
  options: ApexOptions
}

export type CryptoLeftType = {
  title: string
  average: string
  gross: number
  color: string
  subTitle: string
  chart: {
    series: ApexOptions['series']
    color: string
  }
}
export type MainChartDatatype = {
  color: string[]
  label: string[]
  series: ApexOptions['series']
}

export type MainGridType = {
  title: string
  shortName: string
  icon: string
  color: string
  price: string
  gross: number
  chart: MainChartDatatype
}

export type SocialChartType = {
  title: string
  image: string
  gross: number
  total: string
  subTitle: string
  status: string
  sm?: number
  chart: {
    color: string[]
    series: ApexOptions['series']
  }
}

export type ChartCardType = {
  title: string
  total: string
  chart: ChartOptionType
  type: any
}

export type DropdownType = {
  title: string
  icon: string
  price: string
  gross?: string
  color?: string
  option?: string[]
}
export type CustomerSidebarModalProps = {
  modal?: boolean
  toggle: () => void
}

export type CommonChartType = {
  color: string[]
  dropShadowColor: string
  label: string[]
  widgetYSeries: number[]
}

export type DropDownPropsType = {
  Heading: string
  headerClassName?: string
  headingClassName?: string
}

export type CommonHeaderWithDropDownPropsType = {
  heading: string
  headerClassName?: string
  headingClassName?: string
  dropDownList: string[]
  dropDownClass: string
  dropDownIcon: boolean
  caret: boolean
  dropDownToggleClassName?: string
  tag?: string
}

type DataInterFace = {
  imageName: string
  productName: string
  productPrice: number
}

export type ProductContentData = {
  data: DataInterFace
}
