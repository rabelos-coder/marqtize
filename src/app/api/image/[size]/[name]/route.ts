import { ImageResponse } from 'next/og'
import { NextRequest, NextResponse } from 'next/server'

export const dynamic = 'force-dynamic'

export async function GET(
  _request: NextRequest,
  { params }: { params: { name: string; size: any } }
) {
  try {
    let { name, size } = params
    name = `${name}`.trim()
    size = parseInt(`${size}`.trim())

    if (!name) {
      return NextResponse.json(
        { message: 'Name is required!' },
        { status: 400 }
      )
    }
    if (isNaN(size)) {
      return NextResponse.json({ message: 'Invalid size!' }, { status: 400 })
    }

    const Element = await import('@/components/common/LetteredAvatar')
    const LetteredAvatar = Element.LetteredAvatar

    return new ImageResponse(LetteredAvatar({ name, size }), {
      width: size,
      height: size,
    })
  } catch {
    return NextResponse.json(
      { message: 'Failed to generate image!' },
      { status: 500 }
    )
  }
}
