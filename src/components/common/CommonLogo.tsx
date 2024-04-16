import Image from 'next/image'

type PropsType = {
  alignLogo?: string
}

const CommonLogo = ({ alignLogo }: PropsType) => {
  return (
    <a className={`logo ${alignLogo ? alignLogo : ''} `} href="/">
      <Image
        width={100}
        height={37}
        className="img-fluid for-light"
        src={`/assets/images/logo/marqtize_logo.png`}
        alt="logo"
      />
      <Image
        width={100}
        height={37}
        className="img-fluid for-dark"
        src={`/assets/images/logo/marqtize_logo_dark.png`}
        alt="logo"
      />
    </a>
  )
}

export default CommonLogo
