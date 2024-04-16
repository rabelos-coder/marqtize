import dynamic from 'next/dynamic'
import { useState } from 'react'

const NavCustomizer = dynamic(() => import('./NavCustomizer'))
const TabCustomizer = dynamic(() => import('./TabCustomizer'))

const ThemeCustomizer = () => {
  const [selected, setSelected] = useState('check-layout')
  const [openCus, setOpenCus] = useState(false)

  const callbackNav = (select: string, open: boolean) => {
    setSelected(select)
    setOpenCus(open)
  }

  return (
    <>
      <div className={`customizer-links ${openCus ? 'open' : ''}`}>
        <NavCustomizer callbackNav={callbackNav} selected={selected} />
      </div>
      <div className={`customizer-contain ${openCus ? 'open' : ''}`}>
        <TabCustomizer selected={selected} callbackNav={callbackNav} />
      </div>
    </>
  )
}

export default ThemeCustomizer
