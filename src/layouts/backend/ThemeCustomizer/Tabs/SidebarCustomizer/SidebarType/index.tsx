import dynamic from 'next/dynamic'
import { useTranslations } from 'next-intl'

import { useCustomizer } from '@/hooks'

const Horizontal = dynamic(() => import('./Horizontal'), { ssr: false })
const Vertical = dynamic(() => import('./Vertical'), { ssr: false })

const SidebarType = () => {
  const { addSidebarLayouts, layout } = useCustomizer()

  const t = useTranslations()

  const handleSidebarType = (type: string) => {
    addSidebarLayouts(type)
  }

  return (
    <div>
      <h6>{t('sidebarType')}</h6>
      <ul className="sidebar-type layout-grid">
        <Vertical handleSidebarType={handleSidebarType} layout={layout} />
        <Horizontal handleSidebarType={handleSidebarType} layout={layout} />
      </ul>
    </div>
  )
}

export default SidebarType
