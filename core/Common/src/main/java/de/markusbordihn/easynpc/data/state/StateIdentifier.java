/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.data.state;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.resources.ResourceLocation;

public final class StateIdentifier {

  public static final String DEFAULT_NAMESPACE = Constants.MOD_ID;
  public static final int MAX_LENGTH = 64;

  private StateIdentifier() {}

  public static ResourceLocation parse(String stateName) {
    if (stateName == null) {
      return null;
    }

    String trimmedStateName = stateName.trim();
    if (trimmedStateName.isEmpty() || trimmedStateName.length() > MAX_LENGTH) {
      return null;
    }

    ResourceLocation stateId =
        trimmedStateName.indexOf(':') < 0
            ? ResourceLocation.tryBuild(DEFAULT_NAMESPACE, trimmedStateName)
            : ResourceLocation.tryParse(trimmedStateName);

    return stateId != null && !stateId.getPath().isEmpty() ? stateId : null;
  }

  public static boolean isValid(String stateName) {
    return parse(stateName) != null;
  }

  public static boolean isValidInput(String stateName) {
    return stateName != null
        && stateName.length() <= MAX_LENGTH
        && stateName
            .chars()
            .allMatch(character -> ResourceLocation.isAllowedInResourceLocation((char) character));
  }
}
