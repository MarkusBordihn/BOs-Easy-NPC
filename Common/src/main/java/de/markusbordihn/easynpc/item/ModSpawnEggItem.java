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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Objects;
import java.util.function.Supplier;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.flag.FeatureFlagSet;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.SpawnEggItem;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.gameevent.GameEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModSpawnEggItem extends SpawnEggItem {

  public static final String SUFFIX = "_spawn_egg";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private final Supplier<? extends EntityType<? extends Mob>> typeSupplier;

  public ModSpawnEggItem(
      Supplier<? extends EntityType<? extends Mob>> entityType, Properties properties) {
    super(null, Constants.FONT_COLOR_RED, Constants.FONT_COLOR_YELLOW, properties);
    this.typeSupplier = entityType;
  }

  public ModSpawnEggItem(EntityType<? extends Mob> entityType, Properties properties) {
    super(null, Constants.FONT_COLOR_RED, Constants.FONT_COLOR_YELLOW, properties);
    this.typeSupplier = () -> entityType;
  }

  @Override
  public Component getName(ItemStack itemStack) {
    String descriptionId = this.getDescriptionId(itemStack);
    if (descriptionId.contains(SUFFIX)) {
      return TextComponent.getTranslatedTextRaw(
          Constants.ITEM_PREFIX + "spawn_egg",
          TextComponent.getTranslatedTextRaw(
              this.getDescriptionId(itemStack)
                  .replace(Constants.ITEM_PREFIX, Constants.ENTITY_PREFIX)
                  .replace(SUFFIX, "")));
    }
    return TextComponent.getTranslatedTextRaw(this.getDescriptionId(itemStack));
  }

  @Override
  public EntityType<?> getType(CompoundTag tag) {
    EntityType<?> type = super.getType(tag);
    return type != null ? type : this.typeSupplier.get();
  }

  @Override
  public FeatureFlagSet requiredFeatures() {
    return this.typeSupplier.get().requiredFeatures();
  }

  @Override
  public InteractionResult useOn(UseOnContext context) {
    Level level = context.getLevel();
    if (!(level instanceof ServerLevel)) {
      return InteractionResult.SUCCESS;
    }
    Player player = context.getPlayer();
    ItemStack itemStack = context.getItemInHand();
    BlockPos blockPos = context.getClickedPos();
    Direction direction = context.getClickedFace();
    BlockState blockState = level.getBlockState(blockPos);
    BlockPos blockPos1;
    if (blockState.getCollisionShape(level, blockPos).isEmpty()) {
      blockPos1 = blockPos;
    } else {
      blockPos1 = blockPos.relative(direction);
    }

    // Spawn the entity based on the spawn egg type.
    EntityType<?> entityType = this.getType(itemStack.getTag());
    Entity entity =
        entityType.spawn(
            (ServerLevel) level,
            itemStack,
            context.getPlayer(),
            blockPos1,
            MobSpawnType.SPAWN_EGG,
            true,
            !Objects.equals(blockPos, blockPos1) && direction == Direction.UP);

    if (entity != null) {
      // Set owner data for the entity if it is an EasyNPC.
      if (entity instanceof EasyNPC<?> easyNPC && player != null) {
        OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
        if (ownerData != null) {
          ownerData.setOwnerUUID(player.getUUID());
        }
      }

      itemStack.shrink(1);
      level.gameEvent(context.getPlayer(), GameEvent.ENTITY_PLACE, blockPos);
    }

    return InteractionResult.CONSUME;
  }
}
