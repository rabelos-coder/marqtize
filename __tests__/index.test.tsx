/**
 * @jest-environment jsdom
 */
import { render, screen } from '@testing-library/react'

import Page from '@/app/tests/page'

describe('Page', () => {
  it('renders a heading', () => {
    render(<Page />)

    const heading = screen.getByRole('heading', {
      level: 1,
      name: /Welcome to Next\.js!/i,
    })

    expect(heading).toBeInTheDocument()
  })
})
