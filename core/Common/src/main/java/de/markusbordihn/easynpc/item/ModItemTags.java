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
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;

public class ModItemTags {

  // Ranged Weapon Tags
  public static final TagKey<Item> RANGED_WEAPON_BOW =
      TagKey.create(
          Registry.ITEM_REGISTRY, new ResourceLocation(Constants.MOD_ID, "ranged_weapon/bow"));
  public static final TagKey<Item> RANGED_WEAPON_CROSSBOW =
      TagKey.create(
          Registry.ITEM_REGISTRY, new ResourceLocation(Constants.MOD_ID, "ranged_weapon/crossbow"));
  public static final TagKey<Item> RANGED_WEAPON_GUN =
      TagKey.create(
          Registry.ITEM_REGISTRY, new ResourceLocation(Constants.MOD_ID, "ranged_weapon/gun"));
}
