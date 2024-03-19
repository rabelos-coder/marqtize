"use client";

import { useTranslations } from "next-intl";
import { FaFacebookF, FaLinkedinIn, FaTwitter } from "react-icons/fa";
import {
  FacebookShareButton,
  LinkedinShareButton,
  TwitterShareButton,
} from "react-share";

export const PostShare = () => {
  const t = useTranslations();

  let url = "#";
  if (typeof window !== "undefined") url = window.location.href;

  return (
    <div className="single-post-meta-links">
      <FacebookShareButton
        url={url}
        title={t("shareWithName", { name: "Facebook" })}
      >
        <FaFacebookF width={18} height={18} />
      </FacebookShareButton>
      <TwitterShareButton
        url={url}
        title={t("shareWithName", { name: "XTwitter" })}
      >
        <FaTwitter width={18} height={18} />
      </TwitterShareButton>
      <LinkedinShareButton
        url={url}
        title={t("shareWithName", { name: "LinkedIn" })}
      >
        <FaLinkedinIn width={18} height={18} />
      </LinkedinShareButton>
    </div>
  );
};
