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

package de.markusbordihn.easynpc.item;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.function.Supplier;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;

public class ModEpicFightSpawnEggItem extends ModSpawnEggItem {

  public ModEpicFightSpawnEggItem(
      final EntityType<? extends Mob> entityType, final Properties properties) {
    super(entityType, properties);
  }

  public ModEpicFightSpawnEggItem(
      final Supplier<? extends EntityType<? extends Mob>> entityType, final Properties properties) {
    super(entityType, properties);
  }

  @Override
  public Component getName(ItemStack itemStack) {
    if (this.getDescriptionId().contains(SUFFIX)) {
      String baseEntityKey =
          this.getDescriptionId()
              .replace(Constants.ITEM_PREFIX, Constants.ENTITY_PREFIX)
              .replace(CompatConstants.MOD_EPIC_FIGHT_PREFIX, "")
              .replace("_raw", "")
              .replace(SUFFIX, "");

      return TextComponent.getTranslatedTextRaw(
          Constants.ITEM_PREFIX + "epic_fight_spawn_egg",
          TextComponent.getTranslatedTextRaw(baseEntityKey));
    }
    return TextComponent.getTranslatedTextRaw(this.getDescriptionId());
  }
}
