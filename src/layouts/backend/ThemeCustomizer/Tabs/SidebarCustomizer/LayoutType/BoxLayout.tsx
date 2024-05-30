import dynamic from 'next/dynamic'
import { useTranslations } from 'next-intl'

const CommonUL = dynamic(() => import('../CommonUL'), { ssr: false })

type LtrDataType = {
  handleLayout: (item: string) => void
  layout_type: string
}

const BoxLayout = ({ handleLayout, layout_type }: LtrDataType) => {
  const t = useTranslations()

  return (
    <li
      className={`${layout_type === 'box-layout' ? 'active' : ''}`}
      data-attr="box"
      onClick={() => handleLayout('box-layout')}
    >
      <div className="header bg-light">
        <CommonUL />
      </div>
      <div className="body">
        <ul>
          <li className="bg-light sidebar"></li>
          <li className="bg-light body">
            <span className="badge badge-primary">{t('box')}</span>
          </li>
        </ul>
      </div>
    </li>
  )
}

export default BoxLayout
