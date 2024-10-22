/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.easynpc.server.player;

import com.mojang.authlib.GameProfile;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.stats.Stat;

public class FakePlayer extends ServerPlayer {

  public FakePlayer(final ServerLevel level, final BlockPos blockPos) {
    super(level.getServer(), level, new GameProfile(UUID.randomUUID(), "FakePlayer"));
    this.getAdvancements().stopListening();
    this.setPos(blockPos.getX(), blockPos.getY(), blockPos.getZ());
  }

  public static boolean isInvalidFakePlayer(final FakePlayer fakePlayer) {
    return !(fakePlayer instanceof FakePlayer) || !fakePlayer.isAlive();
  }

  public FakePlayer updatePosition(final ServerLevel level, final BlockPos blockPos) {
    if (this.level() != level) {
      this.setLevel(level);
    } else if (this.blockPosition().distSqr(blockPos) > 1.0D) {
      this.setPos(blockPos.getX(), blockPos.getY(), blockPos.getZ());
    }
    return this;
  }

  @Override
  public void displayClientMessage(final Component chatComponent, final boolean actionBar) {
    // Suppress chat messages
  }

  @Override
  public void awardStat(final Stat stat, final int increment) {
    // Suppress stat award
  }

  @Override
  public void tick() {
    // Suppress fake player tick
  }

  @Override
  public void doTick() {
    // Suppress fake player tick
  }
}
