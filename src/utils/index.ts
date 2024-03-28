import { Dirent, readdirSync } from 'fs'
import { join } from 'path'

/**
 * Functions that uses node's fs modules must be declared and exported from this file.
 *
 * NOTE: Do not import any function from this file in the frontend app TSX, only TS like API routes and other Next.js pages.
 */

/**
 * Recursively scans a directory and returns an array of directory entries.
 *
 * @param {string} path - the path of the directory to scan
 * @return {Dirent[]} an array of directory entries
 */
export function recursiveScanDir(path: string): Dirent[] {
  const items = readdirSync(path, { withFileTypes: true }).flatMap((item) => {
    item.path = join(path, item.name)
    if (item.isDirectory()) {
      return recursiveScanDir(item.path)
    }

    return item
  })

  return items
}
