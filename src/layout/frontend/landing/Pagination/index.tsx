'use client'

import { Link } from '@/navigation'
import { PaginationMeta } from '@/types/common'

type PaginationProps = {
  meta?: PaginationMeta | null | undefined
}

export const Pagination = ({ meta }: PaginationProps) => {
  if (!meta) return <></>

  const { lastPage, currentPage, prev, next } = meta

  return (
    lastPage > 1 && (
      <nav>
        <ul className="pagination pagination-blog justify-content-center">
          {prev !== null ? (
            <li className="page-item">
              <Link className="page-link" href={`/blog/${prev}`}>
                <span aria-hidden="true">«</span>
              </Link>
            </li>
          ) : (
            <li className="page-item disabled">
              <Link
                className="page-link"
                href="#!"
                onClick={(e) => e.preventDefault()}
              >
                <span aria-hidden="true">«</span>
              </Link>
            </li>
          )}
          {new Array(lastPage >= 3 ? 3 : lastPage).fill(0).map((_, index) => (
            <li
              className={`page-item ${
                currentPage === index + 1 ? 'active' : ''
              }`}
              key={index}
            >
              <Link className="page-link" href={`/blog/${index + 1}`}>
                {index + 1}
              </Link>
            </li>
          ))}
          {lastPage > 3 && (
            <>
              <li className="page-item disabled">
                <Link
                  className="page-link"
                  href="#!"
                  onClick={(e) => e.preventDefault()}
                >
                  ...
                </Link>
              </li>
              <li className="page-item">
                <Link className="page-link" href={`/blog/${lastPage}`}>
                  {lastPage}
                </Link>
              </li>
            </>
          )}

          {next !== null ? (
            <li className="page-item">
              <Link className="page-link" href={`/blog/${next}`}>
                <span aria-hidden="true">»</span>
              </Link>
            </li>
          ) : (
            <li className="page-item disabled">
              <Link
                className="page-link"
                href="#!"
                onClick={(e) => e.preventDefault()}
              >
                <span aria-hidden="true">»</span>
              </Link>
            </li>
          )}
        </ul>
      </nav>
    )
  )
}
