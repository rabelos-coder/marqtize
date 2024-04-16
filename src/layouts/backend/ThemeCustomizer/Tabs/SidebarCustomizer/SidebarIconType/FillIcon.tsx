import dynamic from 'next/dynamic'
import { useTranslations } from 'next-intl'

const CommonUL = dynamic(() => import('../CommonUL'))

type varTypes = {
  handleSideBarIconType: (data: string) => void
  sideBarIconType: string
}

const FillIcon = ({ handleSideBarIconType, sideBarIconType }: varTypes) => {
  const t = useTranslations()

  return (
    <li
      data-attr="fill-svg"
      className={`border-0 ${sideBarIconType === 'fill-svg' ? 'active' : ''}`}
      onClick={() => handleSideBarIconType('fill-svg')}
    >
      <div className="header bg-light">
        <CommonUL />
      </div>
      <div className="body">
        <div className="body bg-light">
          <span className="badge badge-primary">{t('fill')}</span>
        </div>
      </div>
    </li>
  )
}

export default FillIcon
