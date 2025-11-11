/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SkinHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private SkinHandler() {}

  public static boolean setSkin(EasyNPC<?> easyNPC, SkinDataEntry skinDataEntry) {
    if (easyNPC == null || skinDataEntry == null) {
      log.error("Unable to set skin for {} with entry {}", easyNPC, skinDataEntry);
      return false;
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      log.error("Unable to get skin data for {}", easyNPC);
      return false;
    }

    // Set skin data entry directly to preserve all fields
    log.debug("[{}] Setting skin to {}", easyNPC, skinDataEntry);
    skinData.setSkinDataEntry(skinDataEntry);

    // Handle variant for DEFAULT skin type
    if (skinDataEntry.type() == SkinType.DEFAULT && !skinDataEntry.name().isEmpty()) {
      VariantDataCapable<?> variantData = skinData.getEasyNPCVariantData();
      if (variantData != null) {
        variantData.setSkinVariantType(skinDataEntry.name());
      }
    }

    return true;
  }
}
