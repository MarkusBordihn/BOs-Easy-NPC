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

package de.markusbordihn.easynpc.data.objective.factory;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.Ingredient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

final class TargetItemResolver {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private TargetItemResolver() {}

  static Ingredient resolveTargetItems(ObjectiveDataEntry objectiveDataEntry) {
    String targetItemTag = objectiveDataEntry.getTargetItemTag();
    if (targetItemTag == null || targetItemTag.isEmpty()) {
      return null;
    }

    if (targetItemTag.startsWith("#")) {
      ResourceLocation tagLocation = ResourceLocation.tryParse(targetItemTag.substring(1));
      if (tagLocation == null) {
        log.debug("Unable to parse item tag {} for {}!", targetItemTag, objectiveDataEntry);
        return null;
      }

      return Ingredient.of(TagKey.create(Registries.ITEM, tagLocation));
    }

    ResourceLocation itemLocation = ResourceLocation.tryParse(targetItemTag);
    Item item = itemLocation != null ? BuiltInRegistries.ITEM.get(itemLocation) : Items.AIR;
    if (item == Items.AIR) {
      log.debug("Unable to find item {} for {}!", targetItemTag, objectiveDataEntry);
      return null;
    }

    return Ingredient.of(item);
  }
}
