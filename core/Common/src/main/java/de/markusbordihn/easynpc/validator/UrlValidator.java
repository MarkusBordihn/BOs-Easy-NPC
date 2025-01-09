/*
 * Copyright 2022 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.validator;

import de.markusbordihn.easynpc.Constants;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class UrlValidator {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String[] FORBIDDEN_EXTENSIONS = {
    ".exe",
    ".msi",
    ".dmg",
    ".jar",
    ".bat",
    ".cmd",
    ".com",
    ".pif",
    ".scr",
    ".cpl",
    ".msc",
    ".jar",
    ".app",
    ".sh",
    ".vb",
    ".vbs",
    ".vbe",
    ".js",
    ".jse",
    ".ws",
    ".wsc",
    ".wsh",
    ".ps1",
    ".ps1xml",
    ".ps2",
    ".ps2xml",
    ".psc1",
    ".psc2",
    ".msh",
    ".msh1",
    ".msh2",
    ".mshxml",
    ".msh1xml",
    ".msh2xml",
    ".scf",
    ".lnk",
    ".inf",
    ".reg",
    ".dll",
    ".sys",
    ".drv",
    ".cpl",
    ".ocx",
    ".ax",
    ".spl",
    ".scr",
    ".mui",
    ".dmp",
    ".sys",
    ".cpl",
    ".ocx",
    ".ax",
    ".spl",
    ".scr",
    ".mui",
    ".dmp",
    ".msc",
    ".msp",
    ".msu",
    ".paf",
    ".zip",
    ".rar",
    ".7z",
    ".tar",
    ".gz",
    ".tgz",
  };

  private UrlValidator() {}

  public static boolean isValidUrl(String url) {
    // Basic URL validation.
    if (url == null
        || url.isEmpty()
        || (!url.startsWith("http://") && !url.startsWith("https://"))) {
      if (url != null && !url.isEmpty()) {
        log.error("Invalid URL: {}", url);
      }
      return false;
    }

    // Check for forbidden extensions, to prevent downloading of malicious files.
    for (String extension : FORBIDDEN_EXTENSIONS) {
      if (url.endsWith(extension)) {
        log.error("Forbidden extension found in URL: {}", url);
        return false;
      }
    }

    // Check for valid URL format according to RFC 2396.
    try {
      new URL(url).toURI();
    } catch (MalformedURLException | URISyntaxException e) {
      log.error("Invalid URL format: {}", url);
      return false;
    }
    return true;
  }
}
