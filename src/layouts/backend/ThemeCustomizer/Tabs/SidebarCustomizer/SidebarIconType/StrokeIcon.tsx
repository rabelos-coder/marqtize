import dynamic from 'next/dynamic'
import { useTranslations } from 'next-intl'

const CommonUL = dynamic(() => import('../CommonUL'), { ssr: false })

type varType = {
  handleSideBarIconType: (data: string) => void
  sideBarIconType: string
}

const StrokeIcon = ({ handleSideBarIconType, sideBarIconType }: varType) => {
  const t = useTranslations()

  return (
    <li
      data-attr="stroke-svg"
      className={`normal-sidebar border-0 ${sideBarIconType === 'stroke-svg' ? 'active' : ''}`}
      onClick={() => handleSideBarIconType('stroke-svg')}
    >
      <div className="header bg-light">
        <CommonUL />
      </div>
      <div className="body">
        <div className="body bg-light">
          <span className="badge badge-primary">{t('stroke')}</span>
        </div>
      </div>
    </li>
  )
}

export default StrokeIcon
