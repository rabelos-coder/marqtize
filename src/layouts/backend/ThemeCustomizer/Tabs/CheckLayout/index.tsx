import { useTranslations } from 'use-intl'

import CheckLayoutData from '@/configs/customizer'
import { useCustomizer } from '@/hooks'

const CheckLayout = () => {
  const { setLayoutName } = useCustomizer()
  const t = useTranslations()

  const handlePageLayouts = (type: string) => {
    setLayoutName(type.toLowerCase().replace(' ', ''))
  }

  return (
    <ul className="sidebar-type layout-grid layout-types">
      {CheckLayoutData &&
        CheckLayoutData.map((item, index) => (
          <li
            key={index}
            data-attr={item.attr}
            className={`${item.class ? item.class : ''}`}
            onClick={() => {
              handlePageLayouts(item.slug)
            }}
          >
            <div className="layout-img">
              {/*  eslint-disable-next-line @next/next/no-img-element */}
              <img
                src={`/assets/images/${item.image}`}
                className="img-fluid"
                alt="layout Type"
              />
              <h6>{t(item.title)}</h6>
            </div>
          </li>
        ))}
    </ul>
  )
}

export default CheckLayout
