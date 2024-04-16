'use client'

import { createContext } from 'react'

import { CustomizerContextType } from '@/types/customizer'

const CustomizerContext = createContext<CustomizerContextType>({
  layout: '',
  sidebarIconType: '',
  layoutName: '',
  mixLayout: false,
  toggleIcon: false,
  mix_background_layout: '',
  sidebarResponsive: false,
  IsOpen: false,
  setIsClose: () => {},
  setLayoutName: () => {},
  toggleSidebar: () => {},
  addLayout: () => {},
  setLayout: () => {},
  setMixLayout: () => {},
  toggleSidebarResponsive: () => {},
  setToggleIcon: () => {},
  addSidebarLayouts: () => {},
  addSidebarIconType: () => {},
  setSidebarResponsive: () => {},
  addColor: () => {},
  addMixBackgroundLayout: () => {},
})

export default CustomizerContext
