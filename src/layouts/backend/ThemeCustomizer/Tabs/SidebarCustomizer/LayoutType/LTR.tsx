import dynamic from 'next/dynamic'
import { useTranslations } from 'next-intl'

const CommonUL = dynamic(() => import('../CommonUL'))

type LtrDataType = {
  handleLayout: (item: string) => void
  layout_type: string
}

const LTR = ({ handleLayout, layout_type }: LtrDataType) => {
  const t = useTranslations()

  return (
    <li
      className={`${layout_type === 'ltr' ? 'active' : ''} border-0`}
      data-attr="ltr"
      onClick={() => {
        handleLayout('ltr')
      }}
    >
      <div className="header bg-light">
        <CommonUL />
      </div>
      <div className="body">
        <ul>
          <li className="bg-light sidebar"></li>
          <li className="bg-light body">
            <span className="badge badge-primary">{t('ltr')}</span>
          </li>
        </ul>
      </div>
    </li>
  )
}

export default LTR
