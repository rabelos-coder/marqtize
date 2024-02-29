import Image from "next/image";
import React from "react";

import { Link } from "@/navigation";

type propsType = {
  alignLogo?: string;
};

export const CommonLogo = ({ alignLogo }: propsType) => {
  return (
    <Link className={`logo ${alignLogo ? alignLogo : ""} `} href="/">
      <Image
        width={121}
        height={35}
        className="img-fluid for-light"
        src={`/assets/images/logo/logo.png`}
        alt="looginpage"
      />
      <Image
        width={121}
        height={35}
        className="img-fluid for-dark"
        src={`/assets/images/logo/logo_dark.png`}
        alt="looginpage"
      />
    </Link>
  );
};
