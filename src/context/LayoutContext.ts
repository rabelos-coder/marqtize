'use client'

import { createContext } from 'react'

import { LayoutContextType } from '@/types/layout'

const LayoutContext = createContext<LayoutContextType>({
  searchableMenu: [],
  setSearchableMenu: () => {},
  bookmarkList: [],
  setBookmarkList: () => {},
  searchIcon: false,
  sideBarToggle: false,
  setSideBarToggle: () => {},
  pinnedMenu: [],
  setSearchIcon: () => {},
  bookMarkClass: false,
  setBookMarkClass: () => {},
})

export default LayoutContext
