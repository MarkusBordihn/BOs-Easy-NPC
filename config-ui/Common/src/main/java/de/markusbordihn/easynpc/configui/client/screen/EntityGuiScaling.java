/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen;

import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Fairy;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.AllayRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.CatRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.ChickenRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.CreeperRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.HorseRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.IronGolemRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.PigRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.VexRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.WitchRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.WolfRaw;
import java.util.HashMap;
import java.util.Map;

public class EntityGuiScaling {

  private static final int DEFAULT_SCALING = 45;
  private static final Map<Class<?>, Integer> SCALING_MAP = new HashMap<>();

  static {
    SCALING_MAP.put(AllayRaw.class, 100);
    SCALING_MAP.put(CatRaw.class, 60);
    SCALING_MAP.put(ChickenRaw.class, 80);
    SCALING_MAP.put(CreeperRaw.class, 65);
    SCALING_MAP.put(Fairy.class, 100);
    SCALING_MAP.put(HorseRaw.class, 40);
    SCALING_MAP.put(IronGolemRaw.class, 40);
    SCALING_MAP.put(PigRaw.class, 60);
    SCALING_MAP.put(VexRaw.class, 100);
    SCALING_MAP.put(WitchRaw.class, 55);
    SCALING_MAP.put(WolfRaw.class, 60);
  }

  private EntityGuiScaling() {}

  public static int getScaling(Object entity) {
    if (entity == null) {
      return DEFAULT_SCALING;
    }

    for (Map.Entry<Class<?>, Integer> entry : SCALING_MAP.entrySet()) {
      if (entry.getKey().isInstance(entity)) {
        return entry.getValue();
      }
    }

    return DEFAULT_SCALING;
  }
}
