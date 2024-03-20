import '@testing-library/jest-dom'

import { render, screen } from '@testing-library/react'

import Home from '../src/app/[locale]/page'

describe('Home', () => {
  it('renders a heading', () => {
    render(<Home />)

    const heading = screen.getByRole('heading', {
      name: /Construindo o futuro com inovação e alcançando resultados excepcionais/i,
    })

    expect(heading).toBeInTheDocument()
  })
})
