'use client'

import { useEffect, useState } from 'react'
import { HiOutlineMoon, HiOutlineSun } from 'react-icons/hi2'

import { useAppDispatch, useAppSelector } from '@/hooks'
import { setTheme } from '@/store/slices/themeSlice'

const MoonLight = () => {
  const [darkMode, setDarkMode] = useState(false)

  const dispatch = useAppDispatch()
  const { theme } = useAppSelector((state) => state.theme)

  const DarkModeHandler = (dark: boolean) => {
    if (dark) {
      document.body.classList.remove('dark-only')
      document.body.classList.add('light-only')
      setDarkMode(!darkMode)
      dispatch(setTheme('light'))
    } else {
      document.body.classList.remove('light-only')
      document.body.classList.add('dark-only')
      setDarkMode(!darkMode)
      dispatch(setTheme('dark'))
    }
  }

  useEffect(() => {
    if (theme === 'light') {
      document.body.classList.remove('dark-only')
      document.body.classList.add('light-only')
      setDarkMode(false)
    } else {
      document.body.classList.remove('light-only')
      document.body.classList.add('dark-only')
      setDarkMode(true)
    }
  }, [theme])

  return (
    <li>
      <div
        className={`mode ${darkMode ? 'active' : ''}`}
        onClick={() => DarkModeHandler(darkMode)}
      >
        {darkMode ? (
          <HiOutlineSun className="stroke-icon" />
        ) : (
          <HiOutlineMoon className="stroke-icon" />
        )}
      </div>
    </li>
  )
}

export default MoonLight
