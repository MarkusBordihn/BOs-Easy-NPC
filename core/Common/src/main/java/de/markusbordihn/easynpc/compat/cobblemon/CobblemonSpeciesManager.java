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

package de.markusbordihn.easynpc.compat.cobblemon;

import de.markusbordihn.easynpc.compat.CompatConstants;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.minecraft.resources.ResourceLocation;

public final class CobblemonSpeciesManager {

  public static final String INTEGRATION_ID = CompatConstants.MOD_COBBLEMON_ID;

  public static final String DEFAULT_MODEL = INTEGRATION_ID + ":ditto";

  public static final String VARIANT_FEMALE = "female";
  public static final String VARIANT_SHINY = "shiny";

  private static final List<String> VARIANT_TOKENS = List.of(VARIANT_FEMALE, VARIANT_SHINY);

  private CobblemonSpeciesManager() {}

  public static ResourceLocation getBaseSpeciesId(ResourceLocation modelKey) {
    String basePath = stripVariantTokens(modelKey.getPath(), null);
    if (basePath.equals(modelKey.getPath())) {
      return modelKey;
    }
    return ResourceLocation.fromNamespaceAndPath(modelKey.getNamespace(), basePath);
  }

  public static Set<String> getVariantAspects(ResourceLocation modelKey) {
    Set<String> variantAspects = new HashSet<>();
    stripVariantTokens(modelKey.getPath(), variantAspects);
    return variantAspects;
  }

  public static ResourceLocation createVariantKey(
      ResourceLocation speciesId, String... variantTokens) {
    StringBuilder path = new StringBuilder(speciesId.getPath());
    for (String variantToken : variantTokens) {
      path.append('_').append(variantToken);
    }
    return ResourceLocation.fromNamespaceAndPath(speciesId.getNamespace(), path.toString());
  }

  private static String stripVariantTokens(String path, Set<String> collectedTokens) {
    boolean stripped = true;
    while (stripped) {
      stripped = false;
      for (String variantToken : VARIANT_TOKENS) {
        String variantSuffix = '_' + variantToken;
        if (path.endsWith(variantSuffix)) {
          path = path.substring(0, path.length() - variantSuffix.length());
          if (collectedTokens != null) {
            collectedTokens.add(variantToken);
          }
          stripped = true;
        }
      }
    }
    return path;
  }
}
