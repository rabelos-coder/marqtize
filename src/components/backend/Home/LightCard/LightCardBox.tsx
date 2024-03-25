import DropdownCommon from '@/components/common/DropdownCommon'
import SvgIcon from '@/components/common/SvgIcon'

export const LightCardBox = ({ data }: any) => {
  return (
    <div className="light-card balance-card widget-hover">
      <div className="svg-box">
        <SvgIcon className="svg-fill" iconId={data.icon} />
      </div>
      <div>
        <span className="f-light">{data.title}</span>
        <h6 className="mt-1 mb-0">{data.price}</h6>
      </div>
      <div className="ms-auto text-end">
        <DropdownCommon
          dropdownMain={{ className: 'icon-dropdown', direction: 'start' }}
          options={data.option}
          icon
          iconName="icon-more-alt"
          btn={{ tag: 'span', className: 'pointer' }}
        />
        {data.gross && (
          <span className={`d-inline-block mt-1 font-${data.color}`}>
            {data.gross}
          </span>
        )}
      </div>
    </div>
  )
}
