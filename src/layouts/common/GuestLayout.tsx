'use client'

import { useEffect, useState } from 'react'

import Theme from '@/configs/theme'
import { useAppSelector } from '@/hooks'
import { ChildrenProps } from '@/types/common'

/**
 * Renders the guest layout component for the application.
 *
 * The Guest Layout contains some theme settings and components that are shared across the application.
 *
 * @param {ChildrenProps} children - The children components to be rendered within the layout.
 * @return {JSX.Element} The rendered guest layout.
 */
const GuestLayout = ({ children }: ChildrenProps): JSX.Element => {
  const { theme } = useAppSelector((state) => state.theme)
  const default_color = Theme.data.color.primary_color
  const secondary_color = Theme.data.color.secondary_color
  const [colorBackground1, setColorBackground1] = useState(default_color)
  const [colorBackground2, setColorBackground2] = useState(secondary_color)

  useEffect(() => {
    if (typeof document !== 'undefined') {
      document.documentElement.style.setProperty(
        '--theme-default',
        colorBackground1
      )
      document.documentElement.style.setProperty(
        '--theme-secondary',
        colorBackground2
      )
    }
    Theme.data.color.primary_color = colorBackground1
    Theme.data.color.secondary_color = colorBackground2
  }, [
    setColorBackground1,
    setColorBackground2,
    colorBackground1,
    colorBackground2,
  ])

  useEffect(() => {
    if (theme === 'light') {
      document.body.classList.remove('dark-only')
      document.body.classList.add('light-only')
    } else {
      document.body.classList.remove('light-only')
      document.body.classList.add('dark-only')
    }

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme])

  return <>{children}</>
}

export default GuestLayout
