'use client'

import { useTranslations } from 'next-intl'
import { CardBody } from 'reactstrap'

import { NoteLabels } from '@/fake'

import CustomerDetail from './CustomerDetail'

const PaperNoteBody = () => {
  const t = useTranslations()

  return (
    <CardBody className="pt-0">
      <img
        className="banner-img img-fluid"
        src={`/assets/images/dashboard/papernote.jpg`}
        alt="multicolor background"
      />
      <div className="note-content mt-sm-4 mt-2">
        <p>
          Amet minim mollit non deserunt ullamco est sit aliqua dolor do amet
          sint. Velit officia consequat duis enim velit mollit.
        </p>
        <div className="note-labels">
          <ul>
            {NoteLabels.map((item, i) => (
              <li key={i}>
                <span className={`badge badge-light-${item.color}`}>
                  {item.title}
                </span>
              </li>
            ))}
          </ul>
          <div className="last-label">
            <span className="badge badge-light-success">{t('inProgress')}</span>
          </div>
        </div>
        <CustomerDetail />
      </div>
    </CardBody>
  )
}

export default PaperNoteBody
