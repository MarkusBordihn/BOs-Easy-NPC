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

package de.markusbordihn.easynpc.configui.client.sound;

import de.markusbordihn.easynpc.configui.client.screen.components.SelectOption;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;

public class SoundEventManager {

  private static List<SelectOption<String>> soundEventOptions;

  private SoundEventManager() {}

  public static List<SelectOption<String>> getSoundEventOptions() {
    if (soundEventOptions == null) {
      List<Identifier> soundLocations = new ArrayList<>(BuiltInRegistries.SOUND_EVENT.keySet());
      soundLocations.sort(Comparator.comparing(Identifier::toString));

      List<SelectOption<String>> options = new ArrayList<>(soundLocations.size());
      for (Identifier soundLocation : soundLocations) {
        options.add(SelectOption.of(getDisplayLabel(soundLocation), soundLocation.toString()));
      }
      soundEventOptions = List.copyOf(options);
    }

    return soundEventOptions;
  }

  public static boolean isKnownSoundEvent(String soundName) {
    Identifier soundLocation = Identifier.tryParse(soundName);
    return soundLocation != null && BuiltInRegistries.SOUND_EVENT.containsKey(soundLocation);
  }

  private static String getDisplayLabel(Identifier soundLocation) {
    return Identifier.DEFAULT_NAMESPACE.equals(soundLocation.getNamespace())
        ? soundLocation.getPath()
        : soundLocation.toString();
  }
}
