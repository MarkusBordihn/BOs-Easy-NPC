/**
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

package de.markusbordihn.easynpc.entity.npc;

import java.util.EnumMap;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.Util;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.Profession;

public class Villager extends EasyNPCEntity {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // General Information
  public static final String ID = "villager";
  public static final String NAME = "Villager";
  public static final ResourceLocation BASE_TEXTURE =
      new ResourceLocation("textures/entity/villager/villager.png");

  // Professions
  private static final Map<Profession, ResourceLocation> TEXTURE_BY_PROFESSION =
      Util.make(new EnumMap<>(Profession.class), map -> {
        map.put(Profession.NONE, BASE_TEXTURE);
        map.put(Profession.ARMORER,
            new ResourceLocation("textures/entity/villager/profession/armorer.png"));
        map.put(Profession.BUTCHER,
            new ResourceLocation("textures/entity/villager/profession/butcher.png"));
        map.put(Profession.CARTOGRAPHER,
            new ResourceLocation("textures/entity/villager/profession/cartographer.png"));
        map.put(Profession.CLERIC,
            new ResourceLocation("textures/entity/villager/profession/cleric.png"));
        map.put(Profession.FARMER,
            new ResourceLocation("textures/entity/villager/profession/farmer.png"));
        map.put(Profession.FISHERMAN,
            new ResourceLocation("textures/entity/villager/profession/fisherman.png"));
        map.put(Profession.FLETCHER,
            new ResourceLocation("textures/entity/villager/profession/fletcher.png"));
        map.put(Profession.LEATHERWORKER,
            new ResourceLocation("textures/entity/villager/profession/leatherworker.png"));
        map.put(Profession.LIBRARIAN,
            new ResourceLocation("textures/entity/villager/profession/librarian.png"));
        map.put(Profession.MASON,
            new ResourceLocation("textures/entity/villager/profession/mason.png"));
        map.put(Profession.NITWIT,
            new ResourceLocation("textures/entity/villager/profession/nitwit.png"));
        map.put(Profession.SHEPHERD,
            new ResourceLocation("textures/entity/villager/profession/shepherd.png"));
        map.put(Profession.TOOLSMITH,
            new ResourceLocation("textures/entity/villager/profession/toolsmith.png"));
        map.put(Profession.WEAPONSMITH,
            new ResourceLocation("textures/entity/villager/profession/weaponsmith.png"));
      });

  // Variants
  public enum Variant {
    NONE, DESERT, JUNGLE, PLAINS, SAVANNA, SNOW, SWAMP, TAIGA
  }

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(new EnumMap<>(Variant.class), map -> {
        map.put(Variant.NONE, new ResourceLocation(Constants.MOD_ID, "textures/entity/blank.png"));
        map.put(Variant.DESERT, new ResourceLocation("textures/entity/villager/type/desert.png"));
        map.put(Variant.JUNGLE, new ResourceLocation("textures/entity/villager/type/jungle.png"));
        map.put(Variant.PLAINS, new ResourceLocation("textures/entity/villager/type/plains.png"));
        map.put(Variant.SAVANNA, new ResourceLocation("textures/entity/villager/type/savanna.png"));
        map.put(Variant.SNOW, new ResourceLocation("textures/entity/villager/type/snow.png"));
        map.put(Variant.SWAMP, new ResourceLocation("textures/entity/villager/type/swamp.png"));
        map.put(Variant.TAIGA, new ResourceLocation("textures/entity/villager/type/taiga.png"));
      });

  public Villager(EntityType<? extends AbstractVillager> entityType, Level level) {
    super(entityType, level);
    this.setBaseTextureLocation(BASE_TEXTURE);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes().add(Attributes.MOVEMENT_SPEED, 0.5F)
        .add(Attributes.MAX_HEALTH, 16.0D).add(Attributes.ATTACK_DAMAGE, 0.0D);
  }

  @Override
  public boolean hasProfessions() {
    return true;
  }

  @Override
  public Enum<?>[] getProfessions() {
    return Profession.values();
  }

  @Override
  public Enum<?> getDefaultProfession() {
    return Profession.NONE;
  }

  @Override
  public Enum<?> getProfession(String name) {
    return Profession.valueOf(name);
  }

  @Override
  public boolean hasProfession() {
    return true;
  }

  @Override
  public ResourceLocation getProfessionTextureLocation(Enum<?> profession) {
    return TEXTURE_BY_PROFESSION.get(profession);
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.NONE;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public ResourceLocation getTextureLocation(Enum<?> variant) {
    return TEXTURE_BY_VARIANT.get(variant);
  }

}
