'use client'

import { useTranslations } from 'next-intl'

import { StepperHorizontalPropsType } from '@/types/form'

const StepperHorizontal = ({
  mode,
  level,
  setLevel,
  steps,
  disabled,
}: StepperHorizontalPropsType) => {
  const t = useTranslations()

  const data = steps.concat([t('finish')])

  return (
    <div className="stepper-horizontal">
      {data.map((label, index) => (
        <div
          key={index}
          className={`stepper-one  step ${
            level > index + 1 ? 'done active ' : ''
          }`}
        >
          <div
            className="step-circle"
            style={{ cursor: mode === 'update' ? 'pointer' : 'default' }}
            onClick={() =>
              !disabled && mode === 'update' && setLevel(index + 1)
            }
          >
            <span>{index + 1}</span>
          </div>
          <div
            className="step-title"
            style={{ cursor: mode === 'update' ? 'pointer' : 'default' }}
            onClick={() =>
              !disabled && mode === 'update' && setLevel(index + 1)
            }
          >
            {label}
          </div>
          <div className="step-bar-left" />
          <div className="step-bar-right" />
        </div>
      ))}
    </div>
  )
}

export default StepperHorizontal
